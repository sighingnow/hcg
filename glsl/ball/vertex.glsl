#version 430

uniform mat4 model, view, projection;

layout (location = 0) in vec3 position;
out vec4 color;

void main() {
    mat4 mvp = projection * view * model;
    gl_Position = mvp * vec4(position, 1.0);
    color = vec4(clamp(position, 0.0, 1.0), 1.0);
}
