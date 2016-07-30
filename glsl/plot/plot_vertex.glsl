#version 430

uniform mat4 model, view, projection;

layout (location = 0) in vec3 vpos;
out vec4 color;

uniform vec3 lightpos;

const float specular = 0.3;
const float diffuse = 1 - specular;
varying float intensity;
varying vec2 mcpos;

void main() {
    mat4 mvp = projection * view * model;
    gl_Position = mvp * vec4(vpos, 1.0);
    color = vec4(clamp(vpos, 0.0, 1.0), 1.0);
}
