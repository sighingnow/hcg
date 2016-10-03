#version 430

varying vec4 position_v;

// uniform variables
uniform sampler2D sampler;

in vec2 tex_next;
in vec4 color;

void main() {
    gl_FragColor = texture2D(sampler, tex_next.st);
    // gl_FragColor = position; // color;
}
