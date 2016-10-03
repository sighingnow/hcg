#version 430

// attributes
layout (location = 0) in vec3 vertex;
layout (location = 2) in vec2 tex;

// uniform variables
uniform mat4 model, view, projection;

// varying vec4 position_v;

// output
out vec4 color;
out vec2 tex_next;

void main() {
    mat4 mvp = projection * view * model;
    gl_Position = mvp * vec4(vertex, 1.0);
    color = vec4(clamp(vertex, 0.0, 1.0), 1.0);
    tex_next = tex;

    // position_v = vec4(vertex, 1.0) + vec4(0.5, 0.5, 0.5, 0.0);
}

