..\..\..\..\bin\ucode_2014.exe    18.in  ex1
COPY /Y ex1.#umodlinadv_conf  ex1.#18umodlinadv_conf
DEL     ex1.#umodlinadv_conf
rem -----------------------------------------
..\..\..\..\bin\model_linearity_adv.exe  ex1
COPY /Y ex1.#modlinadv_conf   ex1.#18modlinadv_conf
DEL     ex1.#modlinadv_conf
pause
