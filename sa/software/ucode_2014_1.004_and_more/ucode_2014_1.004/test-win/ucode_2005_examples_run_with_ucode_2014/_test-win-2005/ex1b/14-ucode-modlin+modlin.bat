..\..\..\..\bin\ucode_2014 14.in      ex1
COPY /Y ex1.#umodlin ex1.#14umodlin
DEL     ex1.#umodlin
rem -----------------------------------------
..\..\..\..\bin\model_linearity.exe   ex1
COPY /Y ex1.#modlin  ex1.#14modlin
DEL     ex1.#modlin
pause
