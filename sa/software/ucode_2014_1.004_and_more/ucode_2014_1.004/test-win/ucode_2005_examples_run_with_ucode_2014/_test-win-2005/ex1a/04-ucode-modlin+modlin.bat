..\..\..\..\bin\ucode_2014 04.in      ex1
COPY /Y ex1.#umodlin ex1.#04umodlin
DEL     ex1.#umodlin
..\..\..\..\bin\model_linearity.exe   ex1
COPY /Y ex1.#modlin  ex1.#04modlin
DEL     ex1.#modlin
pause
