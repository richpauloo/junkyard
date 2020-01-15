..\..\..\..\bin\residual_analysis.exe  ex1
COPY /Y ex1.#resan ex1.#13uout-resan
DEL     ex1.#resan
..\..\..\..\bin\residual_analysis_adv.exe  ex1
COPY /Y ex1.#resanadv ex1.#13uout-resanadv
DEL     ex1.#resanadv
pause
