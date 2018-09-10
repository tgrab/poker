create table flop
/* r1 r2 - karty na rece
   s1 s2 s3 - karty na stole
   dokladnosc - ilosc test od (0 do 1000)
   zwyciestwa remisy porazki */
    (r1 int2,r2 int2,s1 int2,s2 int2,s3 int2,
     dokladnosc int2,wygral int8,remis int8, przegral int8);
