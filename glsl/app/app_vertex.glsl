#version 430

layout (location = 0) in vec3 vpos;
out vec4 color;

uniform mat4 model, view, projection;

varying vec4 position;

void main() {
    mat4 mvp = projection * view * model;
    gl_Position = mvp * vec4(vpos, 1.0);
    color = vec4(clamp(vpos, 0.0, 1.0), 1.0);
    position = vec4(vpos, 1.0) + vec4(0.5, 0.5, 0.5, 0.0);
}

