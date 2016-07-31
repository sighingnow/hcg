#version 430

layout (location = 0) in vec3 position;
layout (location = 1) in vec2 tex;

out vec4 color;
out vec2 tex_coord;

uniform mat4 model, view, projection;

void main() {
    mat4 mvp = projection * view * model;
    gl_Position = mvp * vec4(position, 1.0);
    color = vec4(clamp(position, 0.0, 1.0), 1.0);
    tex_coord = tex;
}

