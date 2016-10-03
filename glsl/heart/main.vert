#version 430

uniform mat4 model, view, projection;

layout (location = 0) in vec3 vertex;
out vec4 color;

void main() {
    gl_Position = vec4(vertex, 1.0);
    color = vec4(clamp(vertex, 0.0, 1.0), 1.0);
}
