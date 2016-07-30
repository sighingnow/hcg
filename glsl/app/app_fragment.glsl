#version 430

in vec4 color;

varying vec4 position;

void main() {
    gl_FragColor = position; // color;
}
