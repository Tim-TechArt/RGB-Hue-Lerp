// ============================================
// Color Space Conversion & Interpolation
// ============================================

function rgb2hsv(r, g, b) {
    r /= 255; g /= 255; b /= 255;

    const max = Math.max(r, g, b);
    const min = Math.min(r, g, b);
    const d = max - min;

    let h, s, v = max;

    s = max === 0 ? 0 : d / max;

    if (max === min) {
        h = 0;
    } else {
        switch (max) {
            case r: h = (g - b) / d + (g < b ? 6 : 0); break;
            case g: h = (b - r) / d + 2; break;
            case b: h = (r - g) / d + 4; break;
        }
        h /= 6;
    }

    return [h, s, v];
}

function hsv2rgb(h, s, v) {
    let r, g, b;

    const i = Math.floor(h * 6);
    const f = h * 6 - i;
    const p = v * (1 - s);
    const q = v * (1 - f * s);
    const t = v * (1 - (1 - f) * s);

    switch (i % 6) {
        case 0: r = v; g = t; b = p; break;
        case 1: r = q; g = v; b = p; break;
        case 2: r = p; g = v; b = t; break;
        case 3: r = p; g = q; b = v; break;
        case 4: r = t; g = p; b = v; break;
        case 5: r = v; g = p; b = q; break;
    }

    return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

function lerp(a, b, t) {
    return a + (b - a) * t;
}

function lerpRGB(colorA, colorB, t) {
    return [
        Math.round(lerp(colorA[0], colorB[0], t)),
        Math.round(lerp(colorA[1], colorB[1], t)),
        Math.round(lerp(colorA[2], colorB[2], t))
    ];
}

// Hue direction: 'shortest', 'clockwise', 'counter'
let hueDirection = 'shortest';

function lerpHSV(colorA, colorB, t) {
    const hsvA = rgb2hsv(colorA[0], colorA[1], colorA[2]);
    const hsvB = rgb2hsv(colorB[0], colorB[1], colorB[2]);

    let hueA = hsvA[0];
    let hueB = hsvB[0];

    if (hueDirection === 'shortest') {
        // Take shortest path around wheel
        const hueDiff = hueB - hueA;
        if (Math.abs(hueDiff) > 0.5) {
            if (hueDiff > 0) {
                hueA += 1;
            } else {
                hueB += 1;
            }
        }
    } else if (hueDirection === 'clockwise') {
        // Always go clockwise (increasing hue on our wheel)
        if (hueB <= hueA) {
            hueB += 1;
        }
    } else if (hueDirection === 'counter') {
        // Always go counter-clockwise (decreasing hue on our wheel)
        if (hueB >= hueA) {
            hueA += 1;
        }
    }

    let h = lerp(hueA, hueB, t) % 1;
    if (h < 0) h += 1;

    const s = lerp(hsvA[1], hsvB[1], t);
    const v = lerp(hsvA[2], hsvB[2], t);

    return hsv2rgb(h, s, v);
}

function rgbToHex(rgb) {
    const toHex = (n) => {
        const hex = Math.round(n).toString(16);
        return hex.length === 1 ? '0' + hex : hex;
    };
    return `#${toHex(rgb[0])}${toHex(rgb[1])}${toHex(rgb[2])}`;
}

function rgbToCss(rgb) {
    const r = String(rgb[0]).padStart(3, ' ');
    const g = String(rgb[1]).padStart(3, ' ');
    const b = String(rgb[2]).padStart(3, ' ');
    return `rgb(${r}, ${g}, ${b})`;
}

function hsvToCss(rgb) {
    const [h, s, v] = rgb2hsv(rgb[0], rgb[1], rgb[2]);
    const hPad = String(Math.round(h * 255)).padStart(3, ' ');
    const sPad = String(Math.round(s * 255)).padStart(3, ' ');
    const vPad = String(Math.round(v * 255)).padStart(3, ' ');
    return `hsv(${hPad}, ${sPad}, ${vPad})`;
}

// ============================================
// WebGL Setup
// ============================================

const vertexShaderSource = `
    attribute vec2 a_position;
    varying vec2 v_uv;

    void main() {
        v_uv = a_position * 0.5 + 0.5;
        gl_Position = vec4(a_position, 0.0, 1.0);
    }
`;

const fragmentShaderSource = `
    precision highp float;

    varying vec2 v_uv;

    uniform vec3 u_colorA;
    uniform vec3 u_colorB;
    uniform float u_t;
    uniform int u_mode;
    uniform int u_hueDirection; // 0=shortest, 1=clockwise, 2=counter-clockwise

    #define PI 3.14159265359

    vec3 hsv2rgb(vec3 c) {
        vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
        vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
        return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
    }

    vec3 rgb2hsv(vec3 c) {
        vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
        vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
        vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

        float d = q.x - min(q.w, q.y);
        float e = 1.0e-10;
        return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
    }

    void main() {
        vec2 uv = v_uv * 2.0 - 1.0;
        float dist = length(uv);
        float angle = fract(0.25 - atan(uv.y, uv.x) / (2.0 * PI));

        // Scale wheel down to leave room for dots at edges
        float wheelScale = 0.85;
        float wheelDist = dist / wheelScale;
        vec3 wheelColor = hsv2rgb(vec3(angle, clamp(wheelDist, 0.0, 1.0), 1.0));
        float wheelMask = smoothstep(wheelScale, wheelScale - 0.02, dist);

        vec3 hsvA = rgb2hsv(u_colorA);
        vec3 hsvB = rgb2hsv(u_colorB);

        // Scale dot positions to match wheel (clockwise)
        vec2 posA = vec2(cos((0.25 - hsvA.x) * 2.0 * PI), sin((0.25 - hsvA.x) * 2.0 * PI)) * hsvA.y * wheelScale;
        vec2 posB = vec2(cos((0.25 - hsvB.x) * 2.0 * PI), sin((0.25 - hsvB.x) * 2.0 * PI)) * hsvB.y * wheelScale;

        float pathWidth = 0.04;
        vec3 pathColor = vec3(0.0);
        float pathAlpha = 0.0;
        float glowAlpha = 0.0;
        vec3 glowColor = vec3(1.0, 1.0, 1.0); // White glow

        if (u_mode == 0) {
            vec2 ab = posB - posA;
            float len = length(ab);
            vec2 dir = ab / len;
            vec2 toPoint = uv - posA;
            float proj = clamp(dot(toPoint, dir), 0.0, len);
            vec2 closest = posA + dir * proj;
            float distToLine = length(uv - closest);

            // Glow effect - wider, softer
            float glowWidth = 0.12;
            glowAlpha = smoothstep(glowWidth, 0.0, distToLine) * 0.6;

            // Main path
            pathAlpha = smoothstep(pathWidth, pathWidth * 0.3, distToLine);

            float pathT = proj / len;
            pathColor = mix(u_colorA, u_colorB, pathT);
        } else {
            // HSV arc - continuous line approach
            float hueA = hsvA.x;
            float hueB = hsvB.x;

            // Handle hue wraparound based on direction
            if (u_hueDirection == 0) {
                // Shortest path
                float hueDiff = hueB - hueA;
                if (hueDiff > 0.5) {
                    hueA += 1.0;
                } else if (hueDiff < -0.5) {
                    hueB += 1.0;
                }
            } else if (u_hueDirection == 1) {
                // Clockwise (increasing hue on our wheel)
                if (hueB <= hueA) {
                    hueB += 1.0;
                }
            } else {
                // Counter-clockwise (decreasing hue on our wheel)
                if (hueB >= hueA) {
                    hueA += 1.0;
                }
            }

            // Get hue range (ensure minH < maxH)
            float minH = min(hueA, hueB);
            float maxH = max(hueA, hueB);

            // Check if current pixel's angle falls within the arc's hue range
            float pointHue = angle;

            // Also check pointHue + 1.0 for wraparound cases
            float pointHue2 = pointHue + 1.0;

            bool onArc = (pointHue >= minH && pointHue <= maxH) ||
                         (pointHue2 >= minH && pointHue2 <= maxH);

            if (onArc) {
                // Calculate t (0-1) along the arc
                float useHue = (pointHue >= minH && pointHue <= maxH) ? pointHue : pointHue2;
                float arcT = (useHue - minH) / (maxH - minH);

                // Flip if A > B in hue
                if (hueA > hueB) arcT = 1.0 - arcT;

                // Expected saturation at this hue position
                float expectedSat = mix(hsvA.y, hsvB.y, arcT) * wheelScale;
                float distToArc = abs(dist - expectedSat);

                // Glow effect - wider, softer
                float glowWidth = 0.12;
                glowAlpha = smoothstep(glowWidth, 0.0, distToArc) * 0.6;

                // Main path
                pathAlpha = smoothstep(pathWidth, pathWidth * 0.3, distToArc);

                float h = mod(mix(hueA, hueB, arcT), 1.0);
                float s = mix(hsvA.y, hsvB.y, arcT);
                float v = mix(hsvA.z, hsvB.z, arcT);
                pathColor = hsv2rgb(vec3(h, s, v));
            }
        }

        float dotRadius = 0.12;
        float dotBorder = 0.02;

        float distA = length(uv - posA);
        float dotAMask = smoothstep(dotRadius, dotRadius - 0.01, distA);
        float dotABorder = smoothstep(dotRadius + dotBorder, dotRadius + dotBorder - 0.01, distA) - dotAMask;

        float distB = length(uv - posB);
        float dotBMask = smoothstep(dotRadius, dotRadius - 0.01, distB);
        float dotBBorder = smoothstep(dotRadius + dotBorder, dotRadius + dotBorder - 0.01, distB) - dotBMask;

        vec2 posT;
        vec3 colorT;

        if (u_mode == 0) {
            posT = mix(posA, posB, u_t);
            colorT = mix(u_colorA, u_colorB, u_t);
        } else {
            float hueA2 = hsvA.x;
            float hueB2 = hsvB.x;

            // Use same direction logic as arc
            if (u_hueDirection == 0) {
                float hueDiff2 = hueB2 - hueA2;
                if (abs(hueDiff2) > 0.5) {
                    if (hueDiff2 > 0.0) hueA2 += 1.0;
                    else hueB2 += 1.0;
                }
            } else if (u_hueDirection == 1) {
                // Clockwise (increasing hue on our wheel)
                if (hueB2 <= hueA2) hueB2 += 1.0;
            } else {
                // Counter-clockwise (decreasing hue on our wheel)
                if (hueB2 >= hueA2) hueA2 += 1.0;
            }

            float h = mod(mix(hueA2, hueB2, u_t), 1.0);
            float s = mix(hsvA.y, hsvB.y, u_t);
            float v = mix(hsvA.z, hsvB.z, u_t);
            posT = vec2(cos((0.25 - h) * 2.0 * PI), sin((0.25 - h) * 2.0 * PI)) * s * wheelScale;
            colorT = hsv2rgb(vec3(h, s, v));
        }

        float distT = length(uv - posT);
        float dotTRadius = 0.135;
        float dotTMask = smoothstep(dotTRadius, dotTRadius - 0.01, distT);
        float dotTBorder = smoothstep(dotTRadius + dotBorder, dotTRadius + dotBorder - 0.01, distT) - dotTMask;

        vec3 bgColor = vec3(0.043, 0.051, 0.063);
        vec3 finalColor = mix(bgColor, wheelColor, wheelMask);

        // Apply glow first (underneath path)
        finalColor = mix(finalColor, glowColor, glowAlpha * wheelMask);
        // Apply main path on top
        finalColor = mix(finalColor, pathColor, pathAlpha * 0.95);

        finalColor = mix(finalColor, vec3(1.0), dotABorder);
        finalColor = mix(finalColor, u_colorA, dotAMask);

        finalColor = mix(finalColor, vec3(1.0), dotBBorder);
        finalColor = mix(finalColor, u_colorB, dotBMask);

        finalColor = mix(finalColor, vec3(1.0), dotTBorder);
        finalColor = mix(finalColor, colorT, dotTMask);

        gl_FragColor = vec4(finalColor, 1.0);
    }
`;

function createShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);

    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error('Shader compile error:', gl.getShaderInfoLog(shader));
        gl.deleteShader(shader);
        return null;
    }
    return shader;
}

function createProgram(gl, vertexShader, fragmentShader) {
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);

    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Program link error:', gl.getProgramInfoLog(program));
        return null;
    }
    return program;
}

function setupWebGL(canvas, mode) {
    const gl = canvas.getContext('webgl');
    if (!gl) {
        console.error('WebGL not supported');
        return null;
    }

    const vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
    const fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
    const program = createProgram(gl, vertexShader, fragmentShader);

    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        -1, -1,
         1, -1,
        -1,  1,
         1,  1,
    ]), gl.STATIC_DRAW);

    const positionLocation = gl.getAttribLocation(program, 'a_position');

    return {
        gl,
        program,
        positionBuffer,
        positionLocation,
        uniforms: {
            colorA: gl.getUniformLocation(program, 'u_colorA'),
            colorB: gl.getUniformLocation(program, 'u_colorB'),
            t: gl.getUniformLocation(program, 'u_t'),
            mode: gl.getUniformLocation(program, 'u_mode'),
            hueDirection: gl.getUniformLocation(program, 'u_hueDirection'),
        },
        mode
    };
}

function render(ctx, colorA, colorB, t) {
    const { gl, program, positionBuffer, positionLocation, uniforms, mode } = ctx;

    const rect = gl.canvas.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    gl.canvas.width = rect.width * dpr;
    gl.canvas.height = rect.height * dpr;
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);

    gl.useProgram(program);

    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.enableVertexAttribArray(positionLocation);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);

    gl.uniform3f(uniforms.colorA, colorA[0] / 255, colorA[1] / 255, colorA[2] / 255);
    gl.uniform3f(uniforms.colorB, colorB[0] / 255, colorB[1] / 255, colorB[2] / 255);
    gl.uniform1f(uniforms.t, t);
    gl.uniform1i(uniforms.mode, mode);

    // Pass hue direction: 0=shortest, 1=clockwise, 2=counter
    const hueDir = hueDirection === 'clockwise' ? 1 : (hueDirection === 'counter' ? 2 : 0);
    gl.uniform1i(uniforms.hueDirection, hueDir);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
}

// ============================================
// Main App
// ============================================

const rgbCanvas = document.getElementById('rgbCanvas');
const hsvCanvas = document.getElementById('hsvCanvas');

const rgbCtx = setupWebGL(rgbCanvas, 0);
const hsvCtx = setupWebGL(hsvCanvas, 1);

const tSlider = document.getElementById('tSlider');
const tValueDisplay = document.getElementById('tValue');

const swatchA = document.getElementById('swatchA');
const swatchB = document.getElementById('swatchB');
const rgbADisplay = document.getElementById('rgbA');
const rgbBDisplay = document.getElementById('rgbB');
const hsvADisplay = document.getElementById('hsvA');
const hsvBDisplay = document.getElementById('hsvB');

const rgbBlendSwatch = document.getElementById('rgbBlendSwatch');
const hsvBlendSwatch = document.getElementById('hsvBlendSwatch');
const rgbBlendRgb = document.getElementById('rgbBlendRgb');
const rgbBlendHsv = document.getElementById('rgbBlendHsv');
const hsvBlendRgb = document.getElementById('hsvBlendRgb');
const hsvBlendHsv = document.getElementById('hsvBlendHsv');

let colorA = [255, 0, 0];    // Red (hue 0°)
let colorB = [0, 255, 255];  // Cyan (hue 180°) - opposite side of wheel
let t = parseFloat(tSlider.value);

let draggingDot = null; // 'A', 'B', or null

function update() {
    render(rgbCtx, colorA, colorB, t);
    render(hsvCtx, colorA, colorB, t);

    const rgbResult = lerpRGB(colorA, colorB, t);
    const hsvResult = lerpHSV(colorA, colorB, t);

    // Update blend swatches in slider footer
    rgbBlendSwatch.style.backgroundColor = rgbToCss(rgbResult);
    hsvBlendSwatch.style.backgroundColor = rgbToCss(hsvResult);
    rgbBlendRgb.textContent = rgbToCss(rgbResult);
    rgbBlendHsv.textContent = hsvToCss(rgbResult);
    hsvBlendRgb.textContent = rgbToCss(hsvResult);
    hsvBlendHsv.textContent = hsvToCss(hsvResult);

    // Update endpoint swatches and values
    swatchA.style.backgroundColor = rgbToCss(colorA);
    swatchB.style.backgroundColor = rgbToCss(colorB);
    rgbADisplay.textContent = rgbToCss(colorA);
    rgbBDisplay.textContent = rgbToCss(colorB);
    hsvADisplay.textContent = hsvToCss(colorA);
    hsvBDisplay.textContent = hsvToCss(colorB);

    tValueDisplay.textContent = t.toFixed(2);
}

// Get client coordinates from mouse or touch event
function getClientCoords(event) {
    if (event.touches && event.touches.length > 0) {
        return { clientX: event.touches[0].clientX, clientY: event.touches[0].clientY };
    }
    return { clientX: event.clientX, clientY: event.clientY };
}

// Wheel scale - must match shader
const WHEEL_SCALE = 0.85;

// Get color from canvas position
function getColorFromCanvas(canvas, event) {
    const { clientX, clientY } = getClientCoords(event);
    const rect = canvas.getBoundingClientRect();
    const x = ((clientX - rect.left) / rect.width) * 2 - 1;
    const y = -(((clientY - rect.top) / rect.height) * 2 - 1);

    const dist = Math.sqrt(x * x + y * y);
    if (dist > WHEEL_SCALE) return null;

    let hue = 0.25 - Math.atan2(y, x) / (2 * Math.PI);
    if (hue < 0) hue += 1;
    if (hue >= 1) hue -= 1;
    const sat = Math.min(dist / WHEEL_SCALE, 1);

    return hsv2rgb(hue, sat, 1);
}

// Get dot positions for A, B, and T
function getDotPositions(mode) {
    const [hA, sA] = rgb2hsv(colorA[0], colorA[1], colorA[2]);
    const [hB, sB] = rgb2hsv(colorB[0], colorB[1], colorB[2]);

    const angleA = (0.25 - hA) * 2 * Math.PI;
    const posA = {
        x: Math.cos(angleA) * sA * WHEEL_SCALE,
        y: Math.sin(angleA) * sA * WHEEL_SCALE
    };

    const angleB = (0.25 - hB) * 2 * Math.PI;
    const posB = {
        x: Math.cos(angleB) * sB * WHEEL_SCALE,
        y: Math.sin(angleB) * sB * WHEEL_SCALE
    };

    let posT;
    if (mode === 0) {
        // RGB: linear interpolation
        posT = {
            x: posA.x + (posB.x - posA.x) * t,
            y: posA.y + (posB.y - posA.y) * t
        };
    } else {
        // HSV: arc interpolation - must match hueDirection
        let hueA = hA;
        let hueB = hB;

        if (hueDirection === 'shortest') {
            const hueDiff = hueB - hueA;
            if (hueDiff > 0.5) hueA += 1;
            else if (hueDiff < -0.5) hueB += 1;
        } else if (hueDirection === 'clockwise') {
            if (hueB <= hueA) hueB += 1;
        } else if (hueDirection === 'counter') {
            if (hueB >= hueA) hueA += 1;
        }

        const h = ((hueA + (hueB - hueA) * t) % 1 + 1) % 1;
        const s = sA + (sB - sA) * t;
        const angleT = (0.25 - h) * 2 * Math.PI;
        posT = {
            x: Math.cos(angleT) * s * WHEEL_SCALE,
            y: Math.sin(angleT) * s * WHEEL_SCALE
        };
    }

    return { posA, posB, posT, hA, sA, hB, sB };
}

// Check if click/tap is near a dot
function checkDotClick(canvas, event) {
    const { clientX, clientY } = getClientCoords(event);
    const rect = canvas.getBoundingClientRect();
    const x = ((clientX - rect.left) / rect.width) * 2 - 1;
    const y = -(((clientY - rect.top) / rect.height) * 2 - 1);

    const mode = canvas === rgbCanvas ? 0 : 1;
    const { posA, posB, posT } = getDotPositions(mode);

    const distA = Math.sqrt((x - posA.x) ** 2 + (y - posA.y) ** 2);
    const distB = Math.sqrt((x - posB.x) ** 2 + (y - posB.y) ** 2);
    const distT = Math.sqrt((x - posT.x) ** 2 + (y - posT.y) ** 2);

    // Dot radii (T is slightly larger)
    const dotRadius = 0.18;
    const dotRadiusT = 0.19;

    // Check T first (it's on top visually)
    if (distT <= dotRadiusT) {
        return 'T';
    } else if (distA <= dotRadius && distA <= distB) {
        return 'A';
    } else if (distB <= dotRadius) {
        return 'B';
    }

    return null;
}

// Calculate t value from position along the path
function getTFromPosition(canvas, event) {
    const { clientX, clientY } = getClientCoords(event);
    const rect = canvas.getBoundingClientRect();
    const x = ((clientX - rect.left) / rect.width) * 2 - 1;
    const y = -(((clientY - rect.top) / rect.height) * 2 - 1);

    const mode = canvas === rgbCanvas ? 0 : 1;
    const { posA, posB, hA, sA, hB, sB } = getDotPositions(mode);

    if (mode === 0) {
        // RGB: project onto line from A to B
        const abX = posB.x - posA.x;
        const abY = posB.y - posA.y;
        const len = Math.sqrt(abX * abX + abY * abY);
        if (len === 0) return t;

        const apX = x - posA.x;
        const apY = y - posA.y;
        const proj = (apX * abX + apY * abY) / (len * len);
        return Math.max(0, Math.min(1, proj));
    } else {
        // HSV: calculate from angle - must match hueDirection
        let hueA = hA;
        let hueB = hB;

        if (hueDirection === 'shortest') {
            const hueDiff = hueB - hueA;
            if (hueDiff > 0.5) hueA += 1;
            else if (hueDiff < -0.5) hueB += 1;
        } else if (hueDirection === 'clockwise') {
            if (hueB <= hueA) hueB += 1;
        } else if (hueDirection === 'counter') {
            if (hueB >= hueA) hueA += 1;
        }

        let pointHue = 0.25 - Math.atan2(y, x) / (2 * Math.PI);
        if (pointHue < 0) pointHue += 1;
        if (pointHue >= 1) pointHue -= 1;

        // Handle wraparound
        const minH = Math.min(hueA, hueB);
        const maxH = Math.max(hueA, hueB);

        // Try both pointHue and pointHue + 1
        let newT;
        if (pointHue >= minH && pointHue <= maxH) {
            newT = (pointHue - minH) / (maxH - minH);
        } else if (pointHue + 1 >= minH && pointHue + 1 <= maxH) {
            newT = (pointHue + 1 - minH) / (maxH - minH);
        } else {
            return t; // Outside arc range
        }

        // Flip if A > B
        if (hueA > hueB) newT = 1 - newT;
        return Math.max(0, Math.min(1, newT));
    }
}

let activeCanvas = null;

// Handle start of drag (mouse or touch)
function handleDragStart(canvas, e) {
    const clicked = checkDotClick(canvas, e);

    if (clicked) {
        draggingDot = clicked;
        activeCanvas = canvas;
        canvas.style.cursor = 'grabbing';

        if (draggingDot === 'T') {
            // Update t value based on position
            t = getTFromPosition(canvas, e);
            tSlider.value = t;
            update();
        } else {
            // Immediately update color on click/tap
            const newColor = getColorFromCanvas(canvas, e);
            if (newColor) {
                if (draggingDot === 'A') {
                    colorA = newColor;
                } else {
                    colorB = newColor;
                }
                update();
            }
        }
    }
}

// Handle drag move (mouse or touch)
function handleDragMove(e) {
    if (draggingDot && activeCanvas) {
        if (draggingDot === 'T') {
            // Update t value based on position along path
            t = getTFromPosition(activeCanvas, e);
            tSlider.value = t;
            update();
        } else {
            const newColor = getColorFromCanvas(activeCanvas, e);
            if (newColor) {
                if (draggingDot === 'A') {
                    colorA = newColor;
                } else {
                    colorB = newColor;
                }
                update();
            }
        }
    }
}

// Handle end of drag
function handleDragEnd() {
    if (draggingDot) {
        draggingDot = null;
        activeCanvas = null;
        rgbCanvas.style.cursor = 'default';
        hsvCanvas.style.cursor = 'default';
    }
}

// Setup mouse and touch handlers for canvases
function setupCanvasInteraction(canvas) {
    // Mouse events
    canvas.addEventListener('mousedown', (e) => handleDragStart(canvas, e));

    canvas.addEventListener('mousemove', (e) => {
        if (!draggingDot) {
            // Update cursor based on hover
            const hovering = checkDotClick(canvas, e);
            canvas.style.cursor = hovering ? 'grab' : 'default';
        }
    });

    // Touch events
    canvas.addEventListener('touchstart', (e) => {
        handleDragStart(canvas, e);
        if (draggingDot) {
            e.preventDefault(); // Prevent scrolling while dragging
        }
    }, { passive: false });
}

setupCanvasInteraction(rgbCanvas);
setupCanvasInteraction(hsvCanvas);

// Global mouse handlers
document.addEventListener('mousemove', handleDragMove);
document.addEventListener('mouseup', handleDragEnd);

// Global touch handlers
document.addEventListener('touchmove', (e) => {
    if (draggingDot) {
        e.preventDefault(); // Prevent scrolling while dragging
        handleDragMove(e);
    }
}, { passive: false });

document.addEventListener('touchend', handleDragEnd);
document.addEventListener('touchcancel', handleDragEnd);

tSlider.addEventListener('input', (e) => {
    t = parseFloat(e.target.value);
    update();
});

// Hue direction radio buttons
document.querySelectorAll('input[name="hueDirection"]').forEach(radio => {
    radio.addEventListener('change', (e) => {
        hueDirection = e.target.value;
        update();
    });
});

// Settings dropdown toggle
const hueSettingsBtn = document.getElementById('hueSettingsBtn');
const hueSettingsMenu = document.getElementById('hueSettingsMenu');

hueSettingsBtn.addEventListener('click', (e) => {
    e.stopPropagation();
    hueSettingsMenu.classList.toggle('open');
    hueSettingsBtn.classList.toggle('active');
});

// Close dropdown when clicking outside
document.addEventListener('click', (e) => {
    if (!hueSettingsMenu.contains(e.target) && !hueSettingsBtn.contains(e.target)) {
        hueSettingsMenu.classList.remove('open');
        hueSettingsBtn.classList.remove('active');
    }
});

// Handle resize
window.addEventListener('resize', function() {
    update();
    renderCube();
    renderCylinder();
});

// Toggle header scrolled state
function updateHeader() {
    const header = document.querySelector('.header');
    const isScrolled = document.body.scrollTop > 50 || document.documentElement.scrollTop > 50;
    header.classList.toggle('scrolled', isScrolled);
}

// Update header on scroll
window.addEventListener('scroll', updateHeader);

// Initial render
update();
updateHeader();

// ============================================
// 3D RGB Cube Visualization
// ============================================

const cubeCanvas = document.getElementById('cubeCanvas');
let cubeGl, cubeProgram, cubePointProgram, cubeSolidProgram;
let cubeRotationX = 2.66;
let cubeRotationY = -3.14;
let cubeRotationZ = 0;
let cubeZoom = 2.33; // Camera distance (smaller = closer)
const ZOOM_MIN = 2.0;
const ZOOM_MAX = 8.0;
let isDraggingCube = false;
let lastCubeMouseX, lastCubeMouseY;
let lastPinchDistance = 0;


function initCube() {
    cubeGl = cubeCanvas.getContext('webgl');
    if (!cubeGl) {
        console.error('WebGL not supported for cube');
        return;
    }

    // Line shader
    const lineVS = `
        attribute vec3 a_position;
        attribute vec3 a_color;
        uniform mat4 u_matrix;
        varying vec3 v_color;
        void main() {
            gl_Position = u_matrix * vec4(a_position, 1.0);
            v_color = a_color;
        }
    `;

    const lineFS = `
        precision mediump float;
        varying vec3 v_color;
        void main() {
            gl_FragColor = vec4(v_color, 1.0);
        }
    `;

    // Point shader with optional white glow
    const pointVS = `
        attribute vec3 a_position;
        attribute vec3 a_color;
        attribute float a_size;
        attribute float a_hasGlow;
        uniform mat4 u_matrix;
        varying vec3 v_color;
        varying float v_hasGlow;
        void main() {
            gl_Position = u_matrix * vec4(a_position, 1.0);
            gl_PointSize = a_size;
            v_color = a_color;
            v_hasGlow = a_hasGlow;
        }
    `;

    const pointFS = `
        precision mediump float;
        varying vec3 v_color;
        varying float v_hasGlow;
        void main() {
            float dist = length(gl_PointCoord - vec2(0.5));
            if (dist > 0.5) discard;

            float alpha = smoothstep(0.5, 0.47, dist);
            vec3 finalColor = v_color;

            // Solid white ring border for highlighted points
            if (v_hasGlow > 0.5) {
                float innerRadius = 0.42;
                float borderMask = smoothstep(innerRadius - 0.01, innerRadius, dist);
                finalColor = mix(v_color, vec3(1.0), borderMask);
            }

            gl_FragColor = vec4(finalColor, alpha);
        }
    `;

    // Solid cube shader with clipping plane - position IS the color (RGB cube)
    const solidCubeVS = `
        attribute vec3 a_position;
        uniform mat4 u_matrix;
        varying vec3 v_color;
        varying vec3 v_position;
        void main() {
            gl_Position = u_matrix * vec4(a_position, 1.0);
            v_color = a_position; // RGB = position in cube
            v_position = a_position; // Pass position for clipping
        }
    `;

    const solidCubeFS = `
        precision mediump float;
        varying vec3 v_color;
        varying vec3 v_position;
        uniform vec3 u_planePoint;
        uniform vec3 u_planeNormal;
        uniform float u_clipEnabled;
        uniform float u_clipToCube;
        void main() {
            // Clip to cube bounds (0-1 range)
            if (u_clipToCube > 0.5) {
                if (v_position.x < 0.0 || v_position.x > 1.0 ||
                    v_position.y < 0.0 || v_position.y > 1.0 ||
                    v_position.z < 0.0 || v_position.z > 1.0) {
                    discard;
                }
            }
            // Clip fragments on one side of the plane
            if (u_clipEnabled > 0.5) {
                float dist = dot(v_position - u_planePoint, u_planeNormal);
                if (dist > 0.0) discard;
            }
            gl_FragColor = vec4(v_color, 1.0);
        }
    `;

    function compileShader(gl, type, source) {
        const shader = gl.createShader(type);
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            console.error(gl.getShaderInfoLog(shader));
            return null;
        }
        return shader;
    }

    function createProgram(gl, vs, fs) {
        const program = gl.createProgram();
        gl.attachShader(program, compileShader(gl, gl.VERTEX_SHADER, vs));
        gl.attachShader(program, compileShader(gl, gl.FRAGMENT_SHADER, fs));
        gl.linkProgram(program);
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            console.error(gl.getProgramInfoLog(program));
            return null;
        }
        return program;
    }

    cubeProgram = createProgram(cubeGl, lineVS, lineFS);
    cubePointProgram = createProgram(cubeGl, pointVS, pointFS);
    cubeSolidProgram = createProgram(cubeGl, solidCubeVS, solidCubeFS);

    cubeGl.enable(cubeGl.BLEND);
    cubeGl.blendFunc(cubeGl.SRC_ALPHA, cubeGl.ONE_MINUS_SRC_ALPHA);
}

// Column-major matrix multiplication for WebGL
function multiplyMatrices(a, b) {
    const result = new Float32Array(16);
    for (let col = 0; col < 4; col++) {
        for (let row = 0; row < 4; row++) {
            let sum = 0;
            for (let k = 0; k < 4; k++) {
                sum += a[row + k * 4] * b[k + col * 4];
            }
            result[row + col * 4] = sum;
        }
    }
    return result;
}

function perspectiveMatrix(fov, aspect, near, far) {
    const f = 1.0 / Math.tan(fov / 2);
    return new Float32Array([
        f / aspect, 0, 0, 0,
        0, f, 0, 0,
        0, 0, (far + near) / (near - far), -1,
        0, 0, (2 * far * near) / (near - far), 0
    ]);
}

function rotationXMatrix(angle) {
    const c = Math.cos(angle), s = Math.sin(angle);
    return new Float32Array([
        1, 0, 0, 0,
        0, c, s, 0,
        0, -s, c, 0,
        0, 0, 0, 1
    ]);
}

function rotationYMatrix(angle) {
    const c = Math.cos(angle), s = Math.sin(angle);
    return new Float32Array([
        c, 0, -s, 0,
        0, 1, 0, 0,
        s, 0, c, 0,
        0, 0, 0, 1
    ]);
}

function rotationZMatrix(angle) {
    const c = Math.cos(angle), s = Math.sin(angle);
    return new Float32Array([
        c, s, 0, 0,
        -s, c, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    ]);
}

function translationMatrix(x, y, z) {
    return new Float32Array([
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        x, y, z, 1
    ]);
}

// Vector math helpers for plane calculation
function vec3Cross(a, b) {
    return [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0]
    ];
}

function vec3Normalize(v) {
    const len = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
    if (len === 0) return [0, 0, 0];
    return [v[0] / len, v[1] / len, v[2] / len];
}

function vec3Sub(a, b) {
    return [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
}

function vec3Add(a, b) {
    return [a[0] + b[0], a[1] + b[1], a[2] + b[2]];
}

function vec3Scale(v, s) {
    return [v[0] * s, v[1] * s, v[2] * s];
}

function vec3Dot(a, b) {
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

// Generate solid cube faces (6 faces × 2 triangles × 3 vertices = 36 vertices)
// Each vertex position IS the RGB color
function generateSolidCubeVertices() {
    // Cube corners: (r, g, b) where each is 0 or 1
    const corners = [
        [0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0], // Front face (z=0)
        [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1]  // Back face (z=1)
    ];

    // Define 6 faces as indices into corners array
    // Each face has 4 corners, we split into 2 triangles
    const faces = [
        [0, 1, 2, 3], // Front (z=0): black, red, yellow, green
        [5, 4, 7, 6], // Back (z=1): magenta, blue, cyan, white
        [4, 0, 3, 7], // Left (x=0): blue, black, green, cyan
        [1, 5, 6, 2], // Right (x=1): red, magenta, white, yellow
        [3, 2, 6, 7], // Top (y=1): green, yellow, white, cyan
        [4, 5, 1, 0]  // Bottom (y=0): blue, magenta, red, black
    ];

    const vertices = [];

    for (const face of faces) {
        const c0 = corners[face[0]];
        const c1 = corners[face[1]];
        const c2 = corners[face[2]];
        const c3 = corners[face[3]];

        // Triangle 1: c0, c1, c2
        vertices.push(...c0, ...c1, ...c2);
        // Triangle 2: c0, c2, c3
        vertices.push(...c0, ...c2, ...c3);
    }

    return new Float32Array(vertices);
}

// Calculate the plane normal and perpendicular direction for the cross-section
function calculatePlaneVectors(rgbA, rgbB) {
    const abDir = vec3Normalize(vec3Sub(rgbB, rgbA));

    // Find a perpendicular direction
    let up = [0, 1, 0];
    let perpDir = vec3Cross(abDir, up);
    const perpLen = Math.sqrt(perpDir[0]**2 + perpDir[1]**2 + perpDir[2]**2);
    if (perpLen < 0.001) {
        up = [1, 0, 0];
        perpDir = vec3Cross(abDir, up);
    }
    perpDir = vec3Normalize(perpDir);

    // The plane normal is perpendicular to both abDir and perpDir
    // This gives us a plane that contains the A-B line
    const normal = vec3Normalize(vec3Cross(abDir, perpDir));

    return { abDir, perpDir, normal };
}

// Generate cross-section plane vertices (fills the cut surface)
function generateCrossSectionVertices(rgbA, rgbB, resolution = 32) {
    const vertices = [];
    const { abDir, perpDir } = calculatePlaneVectors(rgbA, rgbB);

    // Calculate midpoint and length of A-B
    const mid = vec3Scale(vec3Add(rgbA, rgbB), 0.5);

    // Extend plane to cover the cube (sqrt(3) ≈ 1.73 is diagonal of unit cube)
    const extent = 1.0;

    for (let i = 0; i < resolution; i++) {
        for (let j = 0; j < resolution; j++) {
            // Parameters along each direction
            const tAB1 = -extent + (i / resolution) * (extent * 2);
            const tAB2 = -extent + ((i + 1) / resolution) * (extent * 2);
            const tPerp1 = -extent + (j / resolution) * (extent * 2);
            const tPerp2 = -extent + ((j + 1) / resolution) * (extent * 2);

            // Calculate 4 corners
            const p00 = vec3Add(vec3Add(mid, vec3Scale(abDir, tAB1)), vec3Scale(perpDir, tPerp1));
            const p10 = vec3Add(vec3Add(mid, vec3Scale(abDir, tAB2)), vec3Scale(perpDir, tPerp1));
            const p01 = vec3Add(vec3Add(mid, vec3Scale(abDir, tAB1)), vec3Scale(perpDir, tPerp2));
            const p11 = vec3Add(vec3Add(mid, vec3Scale(abDir, tAB2)), vec3Scale(perpDir, tPerp2));

            // Only include vertices that are inside the cube (0-1 range)
            const inCube = (p) => p[0] >= 0 && p[0] <= 1 && p[1] >= 0 && p[1] <= 1 && p[2] >= 0 && p[2] <= 1;

            // Add triangles (we'll clip in shader or just include all - clipping handles it)
            vertices.push(...p00, ...p10, ...p11);
            vertices.push(...p00, ...p11, ...p01);
        }
    }

    return new Float32Array(vertices);
}

function renderCube() {
    if (!cubeGl) return;

    const gl = cubeGl;
    // Use rgbCanvas size as reference for consistent sizing
    const refRect = rgbCanvas.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    cubeCanvas.width = refRect.width * dpr;
    cubeCanvas.height = refRect.height * dpr;
    cubeCanvas.style.width = refRect.width + 'px';
    cubeCanvas.style.height = refRect.height + 'px';
    gl.viewport(0, 0, cubeCanvas.width, cubeCanvas.height);

    // Enable depth testing for proper 3D rendering
    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);

    gl.clearColor(0.043, 0.051, 0.063, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // Build transformation matrix
    const aspect = cubeCanvas.width / cubeCanvas.height;
    const proj = perspectiveMatrix(Math.PI / 4, aspect, 0.1, 100);
    const view = translationMatrix(0, 0, -cubeZoom);
    const rotX = rotationXMatrix(cubeRotationX);
    const rotY = rotationYMatrix(cubeRotationY);
    const rotZ = rotationZMatrix(cubeRotationZ);
    const center = translationMatrix(-0.5, -0.5, -0.5);

    let matrix = multiplyMatrices(proj, view);
    matrix = multiplyMatrices(matrix, rotY);
    matrix = multiplyMatrices(matrix, rotX);
    matrix = multiplyMatrices(matrix, rotZ);
    matrix = multiplyMatrices(matrix, center);

    // Get colors A and B as RGB 0-1
    const rgbA = [colorA[0] / 255, colorA[1] / 255, colorA[2] / 255];
    const rgbB = [colorB[0] / 255, colorB[1] / 255, colorB[2] / 255];

    // Calculate plane vectors for clipping
    const { normal: planeNormal } = calculatePlaneVectors(rgbA, rgbB);
    const planeMid = vec3Scale(vec3Add(rgbA, rgbB), 0.5);

    // Draw solid cube with clipping (disable blending for opaque colors)
    gl.disable(gl.BLEND);
    if (cubeSolidProgram) {
        gl.useProgram(cubeSolidProgram);

        const solidVertices = generateSolidCubeVertices();
        const solidBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, solidBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, solidVertices, gl.STATIC_DRAW);

        const solidPosLoc = gl.getAttribLocation(cubeSolidProgram, 'a_position');
        gl.enableVertexAttribArray(solidPosLoc);
        gl.vertexAttribPointer(solidPosLoc, 3, gl.FLOAT, false, 0, 0);

        const solidMatrixLoc = gl.getUniformLocation(cubeSolidProgram, 'u_matrix');
        gl.uniformMatrix4fv(solidMatrixLoc, false, matrix);

        const planePointLoc = gl.getUniformLocation(cubeSolidProgram, 'u_planePoint');
        gl.uniform3fv(planePointLoc, planeMid);

        const planeNormalLoc = gl.getUniformLocation(cubeSolidProgram, 'u_planeNormal');
        gl.uniform3fv(planeNormalLoc, planeNormal);

        const clipEnabledLoc = gl.getUniformLocation(cubeSolidProgram, 'u_clipEnabled');
        const clipToCubeLoc = gl.getUniformLocation(cubeSolidProgram, 'u_clipToCube');

        gl.uniform1f(clipEnabledLoc, 1.0); // Enable plane clipping
        gl.uniform1f(clipToCubeLoc, 0.0);  // Cube faces are already bounded

        gl.drawArrays(gl.TRIANGLES, 0, 36);

        gl.uniform1f(clipEnabledLoc, 0.0);
        gl.uniform1f(clipToCubeLoc, 0.0);
    }

    // Draw cube edges (with depth test - occluded by cube)
    const cubeEdges = [
        // Bottom face edges
        0,0,0, 1,0,0,   1,0,0, 1,0,1,   1,0,1, 0,0,1,   0,0,1, 0,0,0,
        // Top face edges
        0,1,0, 1,1,0,   1,1,0, 1,1,1,   1,1,1, 0,1,1,   0,1,1, 0,1,0,
        // Vertical edges
        0,0,0, 0,1,0,   1,0,0, 1,1,0,   1,0,1, 1,1,1,   0,0,1, 0,1,1
    ];
    const edgeColors = cubeEdges.slice();

    gl.useProgram(cubeProgram);

    const posBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, posBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(cubeEdges), gl.STATIC_DRAW);

    const posLoc = gl.getAttribLocation(cubeProgram, 'a_position');
    const colorLoc = gl.getAttribLocation(cubeProgram, 'a_color');
    const matrixLoc = gl.getUniformLocation(cubeProgram, 'u_matrix');

    gl.enableVertexAttribArray(posLoc);
    gl.vertexAttribPointer(posLoc, 3, gl.FLOAT, false, 0, 0);

    const colorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, colorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(edgeColors), gl.STATIC_DRAW);

    gl.enableVertexAttribArray(colorLoc);
    gl.vertexAttribPointer(colorLoc, 3, gl.FLOAT, false, 0, 0);

    gl.uniformMatrix4fv(matrixLoc, false, matrix);

    // Keep depth test enabled - edges only show on visible surfaces
    gl.drawArrays(gl.LINES, 0, 24);

    // Draw line from A to B (no depth write - won't interfere with T point)
    gl.depthMask(false);
    const linePositions = [...rgbA, ...rgbB];
    const lineColors = [...rgbA, ...rgbB];

    const lineBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, lineBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(linePositions), gl.STATIC_DRAW);
    gl.vertexAttribPointer(posLoc, 3, gl.FLOAT, false, 0, 0);

    const lineColorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, lineColorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(lineColors), gl.STATIC_DRAW);
    gl.vertexAttribPointer(colorLoc, 3, gl.FLOAT, false, 0, 0);

    gl.lineWidth(3.0);
    gl.drawArrays(gl.LINES, 0, 2);
    gl.depthMask(true);

    // Draw cross-section plane (no depth write - doesn't occlude points)
    if (cubeSolidProgram) {
        gl.useProgram(cubeSolidProgram);
        gl.depthMask(false); // Don't write to depth buffer

        const crossSectionVertices = generateCrossSectionVertices(rgbA, rgbB, 40);
        const crossSectionBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, crossSectionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, crossSectionVertices, gl.STATIC_DRAW);

        const solidPosLoc = gl.getAttribLocation(cubeSolidProgram, 'a_position');
        gl.enableVertexAttribArray(solidPosLoc);
        gl.vertexAttribPointer(solidPosLoc, 3, gl.FLOAT, false, 0, 0);

        const solidMatrixLoc = gl.getUniformLocation(cubeSolidProgram, 'u_matrix');
        gl.uniformMatrix4fv(solidMatrixLoc, false, matrix);

        const clipEnabledLoc = gl.getUniformLocation(cubeSolidProgram, 'u_clipEnabled');
        const clipToCubeLoc = gl.getUniformLocation(cubeSolidProgram, 'u_clipToCube');
        gl.uniform1f(clipEnabledLoc, 0.0);
        gl.uniform1f(clipToCubeLoc, 1.0); // Clip to cube bounds

        gl.drawArrays(gl.TRIANGLES, 0, crossSectionVertices.length / 3);
        gl.depthMask(true); // Re-enable depth write
    }

    // Draw points A and B (with depth test - cube can occlude them)
    gl.enable(gl.BLEND);
    gl.enable(gl.DEPTH_TEST);
    gl.useProgram(cubePointProgram);

    const rgbT = [
        rgbA[0] + (rgbB[0] - rgbA[0]) * t,
        rgbA[1] + (rgbB[1] - rgbA[1]) * t,
        rgbA[2] + (rgbB[2] - rgbA[2]) * t
    ];

    const pointPosLoc = gl.getAttribLocation(cubePointProgram, 'a_position');
    const pointColorLoc = gl.getAttribLocation(cubePointProgram, 'a_color');
    const pointSizeLoc = gl.getAttribLocation(cubePointProgram, 'a_size');
    const pointGlowLoc = gl.getAttribLocation(cubePointProgram, 'a_hasGlow');
    const pointMatrixLoc = gl.getUniformLocation(cubePointProgram, 'u_matrix');

    // Points A and B (with depth test)
    // Scale dot sizes based on canvas size (512 is reference size)
    const sizeScale = refRect.width / 512;
    const pointsAB_Positions = [...rgbA, ...rgbB];
    const pointsAB_Colors = [...rgbA, ...rgbB];
    const pointsAB_Sizes = [60.0 * dpr * sizeScale, 60.0 * dpr * sizeScale];
    const pointsAB_Glows = [1.0, 1.0];

    const abPosBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abPosBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Positions), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointPosLoc);
    gl.vertexAttribPointer(pointPosLoc, 3, gl.FLOAT, false, 0, 0);

    const abColorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abColorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Colors), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointColorLoc);
    gl.vertexAttribPointer(pointColorLoc, 3, gl.FLOAT, false, 0, 0);

    const abSizeBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abSizeBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Sizes), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointSizeLoc);
    gl.vertexAttribPointer(pointSizeLoc, 1, gl.FLOAT, false, 0, 0);

    const abGlowBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abGlowBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Glows), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointGlowLoc);
    gl.vertexAttribPointer(pointGlowLoc, 1, gl.FLOAT, false, 0, 0);

    gl.uniformMatrix4fv(pointMatrixLoc, false, matrix);
    gl.drawArrays(gl.POINTS, 0, 2);

    // Point T (depth test ON - cube can occlude)
    const tPosBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tPosBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(rgbT), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointPosLoc, 3, gl.FLOAT, false, 0, 0);

    const tColorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tColorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(rgbT), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointColorLoc, 3, gl.FLOAT, false, 0, 0);

    const tSizeBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tSizeBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([70.0 * dpr * sizeScale]), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointSizeLoc, 1, gl.FLOAT, false, 0, 0);

    const tGlowBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tGlowBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([1.0]), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointGlowLoc, 1, gl.FLOAT, false, 0, 0);

    gl.drawArrays(gl.POINTS, 0, 1);

}

// Cube mouse/touch rotation handlers
cubeCanvas.addEventListener('mousedown', (e) => {
    isDraggingCube = true;
    lastCubeMouseX = e.clientX;
    lastCubeMouseY = e.clientY;
    cubeCanvas.style.cursor = 'grabbing';
});

document.addEventListener('mousemove', (e) => {
    if (isDraggingCube) {
        const dx = e.clientX - lastCubeMouseX;
        const dy = e.clientY - lastCubeMouseY;
        cubeRotationY += dx * 0.01;
        cubeRotationX += dy * 0.01;
        lastCubeMouseX = e.clientX;
        lastCubeMouseY = e.clientY;
        document.getElementById('cubeRotX').value = cubeRotationX;
        document.getElementById('cubeRotY').value = cubeRotationY;
        renderCube();
    }
});

document.addEventListener('mouseup', () => {
    isDraggingCube = false;
    cubeCanvas.style.cursor = 'grab';
});

// Scroll wheel zoom for desktop
cubeCanvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    const zoomSpeed = 0.001;
    cubeZoom += e.deltaY * zoomSpeed * cubeZoom; // Proportional zoom
    cubeZoom = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, cubeZoom));
    renderCube();
}, { passive: false });

// Helper to get distance between two touch points
function getPinchDistance(touches) {
    const dx = touches[0].clientX - touches[1].clientX;
    const dy = touches[0].clientY - touches[1].clientY;
    return Math.sqrt(dx * dx + dy * dy);
}

// Touch support for cube rotation and pinch zoom
cubeCanvas.addEventListener('touchstart', (e) => {
    if (e.touches.length === 1) {
        isDraggingCube = true;
        lastCubeMouseX = e.touches[0].clientX;
        lastCubeMouseY = e.touches[0].clientY;
        e.preventDefault();
    } else if (e.touches.length === 2) {
        isDraggingCube = false;
        lastPinchDistance = getPinchDistance(e.touches);
        e.preventDefault();
    }
}, { passive: false });

document.addEventListener('touchmove', (e) => {
    if (e.touches.length === 1 && isDraggingCube) {
        const dx = e.touches[0].clientX - lastCubeMouseX;
        const dy = e.touches[0].clientY - lastCubeMouseY;
        cubeRotationY += dx * 0.01;
        cubeRotationX += dy * 0.01;
        lastCubeMouseX = e.touches[0].clientX;
        lastCubeMouseY = e.touches[0].clientY;
        document.getElementById('cubeRotX').value = cubeRotationX;
        document.getElementById('cubeRotY').value = cubeRotationY;
        renderCube();
    } else if (e.touches.length === 2 && lastPinchDistance > 0) {
        const currentDistance = getPinchDistance(e.touches);
        const delta = lastPinchDistance - currentDistance;
        const zoomSpeed = 0.02;
        cubeZoom += delta * zoomSpeed;
        cubeZoom = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, cubeZoom));
        lastPinchDistance = currentDistance;
        renderCube();
    }
}, { passive: false });

document.addEventListener('touchend', (e) => {
    if (e.touches.length < 2) {
        lastPinchDistance = 0;
    }
    if (e.touches.length === 0) {
        isDraggingCube = false;
    }
});

// Modify the existing update function to also render the cube
const originalUpdate = update;
update = function() {
    originalUpdate();
    renderCube();
};

// Initialize and render cube
initCube();
renderCube();

// ============================================
// 3D HSV Cylinder Visualization
// ============================================

const cylinderCanvas = document.getElementById('cylinderCanvas');
let cylinderGl, cylinderProgram, cylinderPointProgram, cylinderSolidProgram;
let cylinderRotationX = 0.618;
let cylinderRotationY = 0;
let cylinderRotationZ = 0;
let cylinderZoom = 2.1;
let isDraggingCylinder = false;
let lastCylinderMouseX, lastCylinderMouseY;
let lastCylinderPinchDistance = 0;

function initCylinder() {
    cylinderGl = cylinderCanvas.getContext('webgl');
    if (!cylinderGl) {
        console.error('WebGL not supported for cylinder');
        return;
    }

    // Line shader (same as cube)
    const lineVS = `
        attribute vec3 a_position;
        attribute vec3 a_color;
        uniform mat4 u_matrix;
        varying vec3 v_color;
        void main() {
            gl_Position = u_matrix * vec4(a_position, 1.0);
            v_color = a_color;
        }
    `;

    const lineFS = `
        precision mediump float;
        varying vec3 v_color;
        void main() {
            gl_FragColor = vec4(v_color, 1.0);
        }
    `;

    // Point shader with optional white glow
    const pointVS = `
        attribute vec3 a_position;
        attribute vec3 a_color;
        attribute float a_size;
        attribute float a_hasGlow;
        uniform mat4 u_matrix;
        varying vec3 v_color;
        varying float v_hasGlow;
        void main() {
            gl_Position = u_matrix * vec4(a_position, 1.0);
            gl_PointSize = a_size;
            v_color = a_color;
            v_hasGlow = a_hasGlow;
        }
    `;

    const pointFS = `
        precision mediump float;
        varying vec3 v_color;
        varying float v_hasGlow;
        void main() {
            float dist = length(gl_PointCoord - vec2(0.5));
            if (dist > 0.5) discard;

            float alpha = smoothstep(0.5, 0.47, dist);
            vec3 finalColor = v_color;

            // Solid white ring border for highlighted points
            if (v_hasGlow > 0.5) {
                float innerRadius = 0.42;
                float borderMask = smoothstep(innerRadius - 0.01, innerRadius, dist);
                finalColor = mix(v_color, vec3(1.0), borderMask);
            }

            gl_FragColor = vec4(finalColor, alpha);
        }
    `;

    // Solid cylinder shader - position maps to HSV color
    const solidCylinderVS = `
        attribute vec3 a_position;
        uniform mat4 u_matrix;
        varying vec3 v_color;
        varying vec3 v_position;
        void main() {
            gl_Position = u_matrix * vec4(a_position, 1.0);
            v_position = a_position;

            // Convert cylinder position to HSV color
            // x, z are in range [-0.5, 0.5], y is in range [0, 1]
            float hue = fract(0.25 - atan(-a_position.z, a_position.x) / (2.0 * 3.14159265));
            float sat = length(vec2(a_position.x, a_position.z)) * 2.0; // radius 0.5 -> sat 1
            float val = a_position.y; // y from 0 to 1

            // HSV to RGB conversion
            vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
            vec3 p = abs(fract(vec3(hue) + K.xyz) * 6.0 - K.www);
            v_color = val * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), sat);
        }
    `;

    const solidCylinderFS = `
        precision mediump float;
        varying vec3 v_color;
        varying vec3 v_position;
        uniform vec3 u_planePoint;
        uniform vec3 u_planeNormal;
        uniform float u_clipEnabled;

        void main() {
            if (u_clipEnabled > 0.5) {
                float dist = dot(v_position - u_planePoint, u_planeNormal);
                if (dist > 0.0) discard;
            }
            gl_FragColor = vec4(v_color, 1.0);
        }
    `;

    function compileShader(gl, type, source) {
        const shader = gl.createShader(type);
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            console.error(gl.getShaderInfoLog(shader));
            return null;
        }
        return shader;
    }

    function createProgramCyl(gl, vs, fs) {
        const program = gl.createProgram();
        gl.attachShader(program, compileShader(gl, gl.VERTEX_SHADER, vs));
        gl.attachShader(program, compileShader(gl, gl.FRAGMENT_SHADER, fs));
        gl.linkProgram(program);
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            console.error(gl.getProgramInfoLog(program));
            return null;
        }
        return program;
    }

    cylinderProgram = createProgramCyl(cylinderGl, lineVS, lineFS);
    cylinderPointProgram = createProgramCyl(cylinderGl, pointVS, pointFS);
    cylinderSolidProgram = createProgramCyl(cylinderGl, solidCylinderVS, solidCylinderFS);

    cylinderGl.enable(cylinderGl.BLEND);
    cylinderGl.blendFunc(cylinderGl.SRC_ALPHA, cylinderGl.ONE_MINUS_SRC_ALPHA);
}

// Generate cylinder side surface vertices
function generateCylinderVertices(segments, rings) {
    const vertices = [];
    const radius = 0.5;

    for (let ring = 0; ring < rings; ring++) {
        for (let seg = 0; seg < segments; seg++) {
            const y0 = ring / rings;
            const y1 = (ring + 1) / rings;

            const angle0 = (seg / segments) * Math.PI * 2;
            const angle1 = ((seg + 1) / segments) * Math.PI * 2;

            const x0 = Math.cos(angle0) * radius;
            const z0 = Math.sin(angle0) * radius;
            const x1 = Math.cos(angle1) * radius;
            const z1 = Math.sin(angle1) * radius;

            // Two triangles per quad
            vertices.push(x0, y0, z0, x1, y0, z1, x0, y1, z0);
            vertices.push(x1, y0, z1, x1, y1, z1, x0, y1, z0);
        }
    }

    return new Float32Array(vertices);
}

// Generate cylinder top and bottom caps
function generateCylinderCaps(segments) {
    const vertices = [];
    const radius = 0.5;

    // Top cap (y = 1)
    for (let seg = 0; seg < segments; seg++) {
        const angle0 = (seg / segments) * Math.PI * 2;
        const angle1 = ((seg + 1) / segments) * Math.PI * 2;

        const x0 = Math.cos(angle0) * radius;
        const z0 = Math.sin(angle0) * radius;
        const x1 = Math.cos(angle1) * radius;
        const z1 = Math.sin(angle1) * radius;

        vertices.push(0, 1, 0, x0, 1, z0, x1, 1, z1);
    }

    // Bottom cap (y = 0)
    for (let seg = 0; seg < segments; seg++) {
        const angle0 = (seg / segments) * Math.PI * 2;
        const angle1 = ((seg + 1) / segments) * Math.PI * 2;

        const x0 = Math.cos(angle0) * radius;
        const z0 = Math.sin(angle0) * radius;
        const x1 = Math.cos(angle1) * radius;
        const z1 = Math.sin(angle1) * radius;

        vertices.push(0, 0, 0, x1, 0, z1, x0, 0, z0);
    }

    return new Float32Array(vertices);
}

// Generate cylinder wireframe edges
function generateCylinderWireframe(segments) {
    const edges = [];
    const radius = 0.5;

    // Vertical edges
    for (let seg = 0; seg < segments; seg += segments / 8) {
        const angle = (seg / segments) * Math.PI * 2;
        const x = Math.cos(angle) * radius;
        const z = Math.sin(angle) * radius;
        edges.push(x, 0, z, x, 1, z);
    }

    // Top and bottom rings
    for (let seg = 0; seg < segments; seg++) {
        const angle0 = (seg / segments) * Math.PI * 2;
        const angle1 = ((seg + 1) / segments) * Math.PI * 2;

        const x0 = Math.cos(angle0) * radius;
        const z0 = Math.sin(angle0) * radius;
        const x1 = Math.cos(angle1) * radius;
        const z1 = Math.sin(angle1) * radius;

        // Top ring
        edges.push(x0, 1, z0, x1, 1, z1);
        // Bottom ring
        edges.push(x0, 0, z0, x1, 0, z1);
    }

    return edges;
}

// Convert HSV to cylinder position (x, y, z)
function hsvToCylinderPos(h, s, v) {
    const radius = 0.5;
    const angle = (0.25 - h) * Math.PI * 2;
    return [
        Math.cos(angle) * s * radius,
        v,
        -Math.sin(angle) * s * radius
    ];
}

function renderCylinder() {
    if (!cylinderGl) return;

    const gl = cylinderGl;
    // Use rgbCanvas size as reference for consistent sizing
    const refRect = rgbCanvas.getBoundingClientRect();
    const dpr = window.devicePixelRatio || 1;
    cylinderCanvas.width = refRect.width * dpr;
    cylinderCanvas.height = refRect.height * dpr;
    cylinderCanvas.style.width = refRect.width + 'px';
    cylinderCanvas.style.height = refRect.height + 'px';
    gl.viewport(0, 0, cylinderCanvas.width, cylinderCanvas.height);

    gl.enable(gl.DEPTH_TEST);
    gl.depthFunc(gl.LEQUAL);

    gl.clearColor(0.043, 0.051, 0.063, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

    // Build transformation matrix
    const aspect = cylinderCanvas.width / cylinderCanvas.height;
    const proj = perspectiveMatrix(Math.PI / 4, aspect, 0.1, 100);
    const view = translationMatrix(0, 0, -cylinderZoom);
    const rotX = rotationXMatrix(cylinderRotationX);
    const rotY = rotationYMatrix(cylinderRotationY);
    const rotZ = rotationZMatrix(cylinderRotationZ);
    const center = translationMatrix(0, -0.5, 0); // Center vertically

    let matrix = multiplyMatrices(proj, view);
    matrix = multiplyMatrices(matrix, rotX);
    matrix = multiplyMatrices(matrix, rotY);
    matrix = multiplyMatrices(matrix, rotZ);
    matrix = multiplyMatrices(matrix, center);

    // Get HSV values for A and B
    const hsvA = rgb2hsv(colorA[0], colorA[1], colorA[2]);
    const hsvB = rgb2hsv(colorB[0], colorB[1], colorB[2]);

    // Draw solid cylinder (no clipping - HSV arc stays on surface, no muddy middle to reveal)
    gl.disable(gl.BLEND);
    if (cylinderSolidProgram) {
        gl.useProgram(cylinderSolidProgram);

        // Draw cylinder sides
        const sideVertices = generateCylinderVertices(64, 32);
        const sideBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, sideBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, sideVertices, gl.STATIC_DRAW);

        const solidPosLoc = gl.getAttribLocation(cylinderSolidProgram, 'a_position');
        gl.enableVertexAttribArray(solidPosLoc);
        gl.vertexAttribPointer(solidPosLoc, 3, gl.FLOAT, false, 0, 0);

        const solidMatrixLoc = gl.getUniformLocation(cylinderSolidProgram, 'u_matrix');
        gl.uniformMatrix4fv(solidMatrixLoc, false, matrix);

        const clipEnabledLoc = gl.getUniformLocation(cylinderSolidProgram, 'u_clipEnabled');
        gl.uniform1f(clipEnabledLoc, 0.0); // No clipping

        gl.drawArrays(gl.TRIANGLES, 0, sideVertices.length / 3);

        // Draw caps
        const capVertices = generateCylinderCaps(64);
        const capBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, capBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, capVertices, gl.STATIC_DRAW);
        gl.vertexAttribPointer(solidPosLoc, 3, gl.FLOAT, false, 0, 0);

        gl.drawArrays(gl.TRIANGLES, 0, capVertices.length / 3);
    }

    // Calculate hue values for T point positioning (arc is now rendered in shader)
    let hueA = hsvA[0];
    let hueB = hsvB[0];

    if (hueDirection === 'shortest') {
        const hueDiff = hueB - hueA;
        if (hueDiff > 0.5) hueA += 1;
        else if (hueDiff < -0.5) hueB += 1;
    } else if (hueDirection === 'clockwise') {
        if (hueB <= hueA) hueB += 1;
    } else if (hueDirection === 'counter') {
        if (hueB >= hueA) hueA += 1;
    }

    // Draw points A, B, T
    gl.enable(gl.BLEND);
    gl.useProgram(cylinderPointProgram);

    const posA = hsvToCylinderPos(hsvA[0], hsvA[1], hsvA[2]);
    const posB = hsvToCylinderPos(hsvB[0], hsvB[1], hsvB[2]);

    // Interpolated T position
    const tHue = (hueA + (hueB - hueA) * t) % 1;
    const tSat = hsvA[1] + (hsvB[1] - hsvA[1]) * t;
    const tVal = hsvA[2] + (hsvB[2] - hsvA[2]) * t;
    const posT = hsvToCylinderPos(tHue < 0 ? tHue + 1 : tHue, tSat, tVal);

    const rgbA = [colorA[0] / 255, colorA[1] / 255, colorA[2] / 255];
    const rgbB = [colorB[0] / 255, colorB[1] / 255, colorB[2] / 255];
    const rgbT_color = hsv2rgb(tHue < 0 ? tHue + 1 : tHue, tSat, tVal);
    const rgbT = [rgbT_color[0] / 255, rgbT_color[1] / 255, rgbT_color[2] / 255];

    const pointPosLoc = gl.getAttribLocation(cylinderPointProgram, 'a_position');
    const pointColorLoc = gl.getAttribLocation(cylinderPointProgram, 'a_color');
    const pointSizeLoc = gl.getAttribLocation(cylinderPointProgram, 'a_size');
    const pointGlowLoc = gl.getAttribLocation(cylinderPointProgram, 'a_hasGlow');
    const pointMatrixLoc = gl.getUniformLocation(cylinderPointProgram, 'u_matrix');

    // Points A and B (with glow to stand out on colorful surface)
    // Scale dot sizes based on canvas size (512 is reference size)
    const sizeScale = refRect.width / 512;
    const pointsAB_Positions = [...posA, ...posB];
    const pointsAB_Colors = [...rgbA, ...rgbB];
    const pointsAB_Sizes = [60.0 * dpr * sizeScale, 60.0 * dpr * sizeScale];
    const pointsAB_Glows = [1.0, 1.0];

    const abPosBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abPosBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Positions), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointPosLoc);
    gl.vertexAttribPointer(pointPosLoc, 3, gl.FLOAT, false, 0, 0);

    const abColorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abColorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Colors), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointColorLoc);
    gl.vertexAttribPointer(pointColorLoc, 3, gl.FLOAT, false, 0, 0);

    const abSizeBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abSizeBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Sizes), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointSizeLoc);
    gl.vertexAttribPointer(pointSizeLoc, 1, gl.FLOAT, false, 0, 0);

    const abGlowBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, abGlowBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(pointsAB_Glows), gl.STATIC_DRAW);
    gl.enableVertexAttribArray(pointGlowLoc);
    gl.vertexAttribPointer(pointGlowLoc, 1, gl.FLOAT, false, 0, 0);

    gl.uniformMatrix4fv(pointMatrixLoc, false, matrix);
    gl.drawArrays(gl.POINTS, 0, 2);

    // Point T with glow
    const tPosBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tPosBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(posT), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointPosLoc, 3, gl.FLOAT, false, 0, 0);

    const tColorBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tColorBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(rgbT), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointColorLoc, 3, gl.FLOAT, false, 0, 0);

    const tSizeBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tSizeBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([70.0 * dpr * sizeScale]), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointSizeLoc, 1, gl.FLOAT, false, 0, 0);

    const tGlowBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, tGlowBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([1.0]), gl.STATIC_DRAW);
    gl.vertexAttribPointer(pointGlowLoc, 1, gl.FLOAT, false, 0, 0);

    gl.drawArrays(gl.POINTS, 0, 1);

}

// Cylinder mouse/touch rotation handlers
cylinderCanvas.addEventListener('mousedown', (e) => {
    isDraggingCylinder = true;
    lastCylinderMouseX = e.clientX;
    lastCylinderMouseY = e.clientY;
    cylinderCanvas.style.cursor = 'grabbing';
});

document.addEventListener('mousemove', (e) => {
    if (isDraggingCylinder) {
        const dx = e.clientX - lastCylinderMouseX;
        const dy = e.clientY - lastCylinderMouseY;
        cylinderRotationY += dx * 0.01;
        cylinderRotationX += dy * 0.01;
        lastCylinderMouseX = e.clientX;
        lastCylinderMouseY = e.clientY;
        document.getElementById('cylinderRotX').value = cylinderRotationX;
        document.getElementById('cylinderRotY').value = cylinderRotationY;
        renderCylinder();
    }
});

document.addEventListener('mouseup', () => {
    if (isDraggingCylinder) {
        isDraggingCylinder = false;
        cylinderCanvas.style.cursor = 'grab';
    }
});

// Scroll wheel zoom for cylinder
cylinderCanvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    const zoomSpeed = 0.001;
    cylinderZoom += e.deltaY * zoomSpeed * cylinderZoom;
    cylinderZoom = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, cylinderZoom));
    renderCylinder();
}, { passive: false });

// Touch support for cylinder rotation and pinch zoom
cylinderCanvas.addEventListener('touchstart', (e) => {
    if (e.touches.length === 1) {
        isDraggingCylinder = true;
        lastCylinderMouseX = e.touches[0].clientX;
        lastCylinderMouseY = e.touches[0].clientY;
        e.preventDefault();
    } else if (e.touches.length === 2) {
        isDraggingCylinder = false;
        lastCylinderPinchDistance = getPinchDistance(e.touches);
        e.preventDefault();
    }
}, { passive: false });

document.addEventListener('touchmove', (e) => {
    if (e.touches.length === 1 && isDraggingCylinder) {
        const dx = e.touches[0].clientX - lastCylinderMouseX;
        const dy = e.touches[0].clientY - lastCylinderMouseY;
        cylinderRotationY += dx * 0.01;
        cylinderRotationX += dy * 0.01;
        lastCylinderMouseX = e.touches[0].clientX;
        lastCylinderMouseY = e.touches[0].clientY;
        document.getElementById('cylinderRotX').value = cylinderRotationX;
        document.getElementById('cylinderRotY').value = cylinderRotationY;
        renderCylinder();
    } else if (e.touches.length === 2 && lastCylinderPinchDistance > 0) {
        const currentDistance = getPinchDistance(e.touches);
        const delta = lastCylinderPinchDistance - currentDistance;
        const zoomSpeed = 0.02;
        cylinderZoom += delta * zoomSpeed;
        cylinderZoom = Math.max(ZOOM_MIN, Math.min(ZOOM_MAX, cylinderZoom));
        lastCylinderPinchDistance = currentDistance;
        renderCylinder();
    }
}, { passive: false });

document.addEventListener('touchend', (e) => {
    if (e.touches.length < 2) {
        lastCylinderPinchDistance = 0;
    }
    if (e.touches.length === 0) {
        isDraggingCylinder = false;
    }
});

// Hook cylinder rendering into the update function
const originalUpdateWithCube = update;
update = function() {
    originalUpdateWithCube();
    renderCylinder();
};

// Initialize and render cylinder
initCylinder();
renderCylinder();

// Rotation slider event listeners - Cube
document.getElementById('cubeRotX').addEventListener('input', (e) => {
    cubeRotationX = parseFloat(e.target.value);
    renderCube();
});
document.getElementById('cubeRotY').addEventListener('input', (e) => {
    cubeRotationY = parseFloat(e.target.value);
    renderCube();
});
document.getElementById('cubeRotZ').addEventListener('input', (e) => {
    cubeRotationZ = parseFloat(e.target.value);
    renderCube();
});

// Rotation slider event listeners - Cylinder
document.getElementById('cylinderRotX').addEventListener('input', (e) => {
    cylinderRotationX = parseFloat(e.target.value);
    renderCylinder();
});
document.getElementById('cylinderRotY').addEventListener('input', (e) => {
    cylinderRotationY = parseFloat(e.target.value);
    renderCylinder();
});
document.getElementById('cylinderRotZ').addEventListener('input', (e) => {
    cylinderRotationZ = parseFloat(e.target.value);
    renderCylinder();
});
