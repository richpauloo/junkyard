..\..\..\..\bin\ucode_2014 03.in           ex1
COPY /Y ex1.#uout     ex1.#03uout-parest
DEL     ex1.#uout
..\..\..\..\bin\residual_analysis.exe      ex1
COPY /Y ex1.#resan    ex1.#03uout-resan
DEL     ex1.#resan
..\..\..\..\bin\residual_analysis_adv.exe  ex1
COPY /Y ex1.#resanadv ex1.#03uout-resanadv
DEL     ex1.#resanadv
pause
