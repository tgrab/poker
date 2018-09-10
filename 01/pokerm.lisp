;; reprezentacja wektorowa pokera

;karta to liczba od 0 do 51
; po koleji  AS_pik,Krol_pik,...,2_Trefl

(defmacro podzbiory-2(wektor funkcja &optional (dlugosc 52))
 "wektor opisuje liste liczb z ktorych mozna brac podzbiory
  jesli wartosc  indeksie i nie jest nil to i jest brane do podzbiorow 
  dla kazdego takiego pobzbioru wywolywana jest funkcja"
  `(loop for i from 0 to ,dlugosc do
     (loop for j from (1+ i) to ,dlugosc do
       (funcall ,funkcja i j))) )