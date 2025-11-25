

;; Функции вопроса


(
deffunction ask-question (?question $?allowed-values)
   (print ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       	then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (print ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer
)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


;;; RULES. Вопросы


;; ПЕРВЫЙ ВОПРОС
(defrule determ-rok ""
   	(not (rok ?))
   	(not (answer ?))
  	 =>
  	 (assert (rok (yes-or-no-p "Вы слушаете рок (yes/no)? "))))
   
;; ___________________ПРАВАЯ ВЕТКА ________________________________________________________________

(defrule determine-hard-rok ""
  	 (rok  yes)
  	 (not (answer ?))
  	 =>
  	 (assert (hard-rok (yes-or-no-p "Любите ли Вы хард-рок (yes/no)? "))))

(defrule determine-Lordi ""
   	(hard-rok yes)
  	 (not (answer ?))
  	 =>
   	(assert (Lordi (yes-or-no-p "Вы знаете Motörhead (yes/no)? "))))

(defrule determine-Kiss ""
   	(Lordi yes)
  	 (not (answer ?))
   	=>
   	(assert (Kiss (yes-or-no-p "Возможно Вам нравится группа Kiss (yes/no)? "))))

(defrule determine-SektorGaza ""
       (Kiss yes)
       (not (answer ?))
       =>
       (assert (SektorGaza (yes-or-no-p "Вероятно Вы слушаете Bon Jovi (yes/no)? "))))

(defrule determine-Grajdan ""
       (Kiss no)
       (not (answer ?))
       =>
       (assert (Grajdan (yes-or-no-p "Возможно Вам нравится группа Deep Purple (yes/no)? "))))

(defrule determine-ACDC ""
   	(Lordi no)
   	(not (answer ?))
   	=>
   	(assert (ACDC (yes-or-no-p "Понял, скорее всего Вы фанат AC/DC (yes/no)? "))))

(defrule determine-George ""
    (ACDC no)
    (not (answer ?))
    =>
    (assert (George (yes-or-no-p "Может Вы слушаете George Thorogood (yes/no)? "))))

(defrule determine-U2 ""
    (ACDC yes)
    (not (answer ?))
    =>
    (assert (U2 (yes-or-no-p "Может Вы слушаете U2 (yes/no)? "))))

(defrule determine-soft ""
   	(hard-rok no)
	(not (answer ?))
	=>
	(assert (soft (yes-or-no-p "Хм.. Может Вы любите Софт-Рок (yes/no)? "))))

(defrule determine-Music-Soft ""
   	(soft yes)
	(not (answer ?))
	=>
	(assert (Music-Soft (yes-or-no-p "Вы слушаете Пол-а Уильямса (yes/no)? "))))

(defrule determine-Music-Soft3 ""
       (Music-Soft yes)
    (not (answer ?))
    =>
    (assert (Music-Soft3 (yes-or-no-p "Вы слушаете Tracy Chapman (yes/no)? "))))

(defrule determine-Music-Soft4 ""
       (Music-Soft3 yes)
    (not (answer ?))
    =>
    (assert (Music-Soft4 (yes-or-no-p "Вы слушаете TSP (yes/no)? "))))

(defrule determine-Music-Soft5 ""
       (Music-Soft3 no)
    (not (answer ?))
    =>
    (assert (Music-Soft5 (yes-or-no-p "Вы слушаете GooGoo Dolls (yes/no)? "))))

(defrule determine-SoftNew ""
       (Music-Soft no)
    (not (answer ?))
    =>
    (assert (SoftNew (yes-or-no-p "Понял, возможно Вы слушаете группу Santana (yes/no)? "))))


(defrule determine-classic "" ;;life simulator
	(soft no)
	(not (answer ?))
	=>
	(assert (classic (yes-or-no-p "Значит Вы слушаете Классический Рок (yes/no)? "))))

(defrule determine-PinkFloyd "" ;; Free-travel
	(classic yes)
	(not (answer ?))
	=>
	(assert (PinkFloyd (yes-or-no-p "Вы навернека знаете Pink Floyd (yes/no)? "))))

(defrule determine-whatulis "" ;; Powerover
	(classic no)
	(not (answer ?))
	=>
	(assert (whatulis (yes-or-no-p "Странно, возможно Вы любите Aerosmith (yes/no)? "))))
    ;; whatulis yes
(defrule determine-Beatles "" ;;
    (whatulis yes)
    (not (answer ?))
    =>
    (assert (Beatles (yes-or-no-p "Может Вы знаете The Beatels(yes/no)? "))))
(defrule determine-maybeFolk "" ;;-----------
    (whatulis no)
    (not (answer ?))
    =>
    (assert (maybeFolk (yes-or-no-p "Навернека Вы слушаете Фолк-Рок (yes/no)? "))))
(defrule determine-Kalinov "" ;;----------
    (maybeFolk yes)
    (not (answer ?))
    =>
    (assert (Kalinov (yes-or-no-p "Знаете ли Вы группу Калинов Мост (yes/no)? "))))
(defrule determine-Kpop "" ;;----------
    (maybeFolk no)
    (not (answer ?))
    =>
    (assert (Kpop (yes-or-no-p "Может Вы любите K-pop :( (yes/no)? "))))

(defrule determine-BTS "" ;;----------
    (Kpop yes)
    (not (answer ?))
    =>
    (assert (BTS (yes-or-no-p "Навернека Вы знаете группу BTS :((( (yes/no)? "))))

(defrule determine-IDK "" ;;--------
    (Kalinov yes)
    (not (answer ?))
    =>
    (assert (IDK (yes-or-no-p "Круто! А знаете ли Вы The Byrds (yes/no)? "))))
(defrule determine-Nothing "" ;;--------
    (Kpop no)
    (not (answer ?))
    =>
    (assert (Nothing (yes-or-no-p "Хмм... Может тогда Вы знаете Bob Dylan (yes/no)? "))))


;;________________________КОНЕЦ ПРАВОЙ ВЕТКИ ДЕРЕВА __________________________________________________

(defrule determine-metall "" ;; Realworld
   	(rok  no)
   	(not (answer ?))
   	=>
   	(assert (metall (yes-or-no-p "Тогда Вам нравится Металл (yes/no)? "))))

(defrule determine-industrial "" ;;о будущем
	(metall yes)
	(not (answer ?))
   	=>
	(assert (industrial (yes-or-no-p "Вы любитель Индастриала (yes/no)? "))))

(defrule determine-Rammstein "" ;;RTS real time strat
	(industrial yes)
	(not (answer ?))
   	=>
	(assert (Rammstein (yes-or-no-p "Вы любите Раммштайн (yes/no)? "))))

(defrule determine-Marlin ""
    (Rammstein no)
    (not (answer ?))
    =>
    (assert (Marlin (yes-or-no-p "Возможно Вы фанат - Мэрлина Менсона (yes/no)? "))))

(defrule determine-RamYes ""
    (Rammstein yes)
    (not (answer ?))
    =>
    (assert (RamYes (yes-or-no-p "Возможно Вы фанат - Heldmaschine (yes/no)? "))))

(defrule determine-heavyMet "" ;; Weapon вооружения
	(industrial no)
	(not (answer ?))
   	=>
	(assert (heavyMet (yes-or-no-p "Возможно Вы любитель Хэви-Металл (yes/no)? "))))

(defrule determine-Metallica "" ;; konflikt 20 vek
	(heavyMet yes)
	(not (answer ?))
   	=>
	(assert (Metallica (yes-or-no-p "Знаете ли Вы группу - Металлика (yes/no)? "))))

(defrule determine-NotHavy "" ;; konflikt 20 vek
    (heavyMet no)
    (not (answer ?))
       =>
    (assert (NotHavy (yes-or-no-p "Знаете ли Вы группу - Ван Халлена (yes/no)? "))))

(defrule determine-pop "" ;; fantasy
	(metall no)
	(not (answer ?))
   	=>
	(assert (pop (yes-or-no-p "Тогда... Вы любитель попсы (yes/no)? "))))

(defrule determine-KatyP "" ;;RTS
	(pop yes)
	(not (answer ?))
   	=>
	(assert (KatyP (yes-or-no-p "Слушаете ли Вы ХаммАли и Наваи (yes/no)? "))))

(defrule determine-TaylorS ""
	(KatyP no)
	(not (answer ?))
   	=>
	(assert (TaylorS (yes-or-no-p "Навернека Вы слушаете Анну Асти (yes/no)? "))))

(defrule determine-Hanna ""
    (KatyP yes)
    (not (answer ?))
       =>
    (assert (Hanna (yes-or-no-p "Может Вы слушаете 5УТРА (yes/no)? "))))


(defrule determine-Mayle ""
    (TaylorS no)
    (not (answer ?))
       =>
    (assert (Mayle (yes-or-no-p "Может Вы фанат певицы Моя Мишель (yes/no)? "))))


(defrule determine-electro "" ;;Star
	(pop no)
	(not (answer ?))
   	=>
	(assert (electro (yes-or-no-p "Понял, Вы любитель Электро (yes/no)? "))))

(defrule determine-Techno ""
    (electro no)
    (not (answer ?))
       =>
    (assert (Techno  (yes-or-no-p "Может Вы слушаете Техно (yes/no)? "))))

(defrule determine-duftP ""
	(electro yes)
	(not (answer ?))
   	=>
	(assert (duftP  (yes-or-no-p "100% Вы слушаете Duft-Punk (yes/no)? "))))

;;;* ОТВЕТЫ *


;;__________ОТВЕТЫ ПРАВОЙ ВЕТКИ_______________

(defrule Lordi-Kiss-answer1 ""
   	(Lordi yes)
   	(Kiss yes)
    ;;(Grajdan yes)
    (SektorGaza yes)
  	(not (answer ?))
	=>
	(printout t "Рекомендую послушать Вам Guns N' Roses, начать можно с песни Sweet Child O' Mine." crlf)
	(assert (answer "Рекомендую послушать Вам Guns N' Roses, начать можно с песни Sweet Child O' Mine.")))

(defrule Lordi-Kiss-answer9 ""
       (Lordi yes)
       (Kiss yes)
    (SektorGaza no)
      (not (answer ?))
    =>
    (printout t "Рекомендую послушать Lordi - Hard Rock" crlf)
    (assert (answer "Рекомендую послушать Lordi - Hard Rock")))

(defrule Lordi-Kiss-answer2 ""
  	(Lordi yes)
  	(Kiss no)
    (Grajdan no) ;; Deep Purple
  	(not (answer ?))
  	=>
	(printout t "Рекомендую послушать группу KISS, начать можно с песни I Was Made For Loving You, или если Вы любите Финских исполнителей могу предложить Вам Sielun Veljet, с песни Peltirumpu." crlf)
  	(assert (answer "Рекомендую послушать группу KISS, начать можно с песни I Was Made For Loving You, или если Вы любите Финских исполнителей могу предложить Вам Sielun Veljet, с песни Peltirumpu.")))

(defrule Lordi-Kiss-answer5 ""
      (Lordi yes)
      (Kiss no)
    (Grajdan yes)
      (not (answer ?))
      =>
    (printout t "Рекомендую послушать Kevin Rudolf, песню In The City" crlf)
      (assert (answer "Рекомендую послушать Kevin Rudolf, песню In The City")))

(defrule Lordi-ACDC-answer1 ""
     (Lordi no)
       (ACDC yes)
       (U2 yes)
       (not (answer ?))
       =>
    (printout t "Отличный вкус! Возможно Вам так же понравится Motley Crue." crlf)
       (assert (answer "Отличный вкус! Возможно Вам так же понравится Motley Crue.")))

(defrule Lordi-ACDC-answer3 ""
     (Lordi no)
       (ACDC yes)
       (U2 no)
       (not (answer ?))
       =>
    (printout t "Рекомендую послушать песни U2 если не понравится советую послушать Led Zeppelin." crlf)
       (assert (answer "Рекомендую послушать песни U2 если не понравится советую послушать Led Zeppelin.")))

(defrule Lordi-George-answer1 ""
       (Lordi no)
       (ACDC no)
       (George yes)
       (not (answer ?))
       =>
    (printout t "Отлично, предлогаю послушать исполнителя - Slipknot." crlf)
       (assert (answer "Отлично, предлогаю послушать исполнителя - Slipknot.")))

(defrule Lordi-George-answer2 ""
  	 (Lordi no)
     (ACDC no)
  	 (George no)
  	 (not (answer ?))
  	 =>
	(printout t "Рекомендую все же, послушать George Thorogood, если не понравится, могу предложить Вам группу Bon Jovi, с песней You Give Love a Bad Name." crlf)
  	 (assert (answer "Рекомендую все же, послушать ACDC, если не понравится, могу предложить Вам группу Bon Jovi, с песней You Give Love a Bad Name.")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule Music-Soft-answer1 ""
  	(soft yes)
  	(Music-Soft yes)
    (Music-Soft3 yes)
    (Music-Soft4 yes)
   ;; (Music-Soft5 yes)
	(not (answer ?))
	=>
	(printout t "Очень рекомендую послушать группу Eagles, начать с песни Hotel California." crlf)
	(assert (answer "Очень рекомендую послушать группу Eagles, начать с песни Hotel California.")))

(defrule Music-Soft3-answer2 ""
      (soft yes)
      (Music-Soft yes)
    (Music-Soft3 yes)
    (Music-Soft4 no)
    ;;(Music-Soft5 no)
    (not (answer ?))
    =>
    (printout t "Очень рекомендую послушать Sinead O'Connor." crlf)
    (assert (answer "Очень рекомендую послушать Sinead O'Connor.")))

(defrule Music-Soft4-answer1 ""
      (soft yes)
      (Music-Soft yes)
    (Music-Soft3 no)
    ;;(Music-Soft4 yes)
    (Music-Soft5 yes)
    (not (answer ?))
    =>
    (printout t "Очень рекомендую послушать Seal - Kiss by the Rose." crlf)
    (assert (answer "Очень рекомендую послушать Seal - Kiss by the Rose.")))

(defrule Music-Soft4-answer2 ""
      (soft yes)
      (Music-Soft yes)
    (Music-Soft3 yes)
    ;;(Music-Soft4 no)
    (Music-Soft5 no)
    (not (answer ?))
    =>
    (printout t "Очень рекомендую послушать GFS." crlf)
    (assert (answer "Очень рекомендую послушать GFS.")))

(defrule Music-Soft-answer2 ""
  	(soft yes)
  	(SoftNew no) ;; santana
	(not (answer ?))
	=>
	(printout t "Рекомендую послушать Кристофера Кросса, если не понравится предлагаю послушать группу Seals & Crofts." crlf)
	(assert (answer "Рекомендую послушать Кристофера Кросса, если не понравится предлагаю послушать группу Seals & Crofts.")))

(defrule SoftNew-answer1 ""
      (soft yes)
      (SoftNew yes)
    (not (answer ?))
    =>
    (printout t "Круто, рекомендую послушать группу New Radicals - get what you give." crlf)
    (assert (answer "Круто, рекомендую послушать группу New Radicals - get what you give.")))

(defrule classic-answer1 ""
	(soft no)
	(classic yes)
	(PinkFloyd yes)
	(not (answer ?))
	=>
	(printout t "Круто, что Вы слушаете Пинк Флойда, так же могу порекомендовать на подобие - The Rolling Stones - Paint it black." crlf)
	(assert (answer "Круто, что Вы слушаете Пинк Флойда, так же могу порекомендовать на подобие - The Rolling Stones - Paint it black.")))

(defrule classic-answer2 ""
	(soft no)
	(classic yes)
	(PinkFloyd no)
	(not (answer ?))
	=>
	(printout t "Рекомендую послушать хотя бы песню Another Brick on The Wall от Pink Floyd, если не понравилось советую послушать группу Queen, начать можно с популярной песни - Show Must Go On." crlf)
	(assert (answer "Рекомендую послушать хотя бы песню Another Brick on The Wall от Pink Floyd, если не понравилось советую послушать группу Queen, начать можно с популярной песни - Show Must Go On.")))

(defrule whatulis-answer1 ""
	(soft no)
	(classic no)
	(whatulis yes) ;; Beatles
	(not (answer ?))
	=>
	(printout t "Рекомендую послушать песню Tongues от Joywave & KOPPS, песня зашла в саундтреки к игре FIFA 15" crlf)
	(assert (answer "Рекомендую послушать песню Tongues от Joywave & KOPPS, песня зашла в саундтреки к игре FIFA 15")))

(defrule whatulis-answer63 ""
    (soft no)
    (classic no)
    (whatulis no) ;; Beatles
    (not (answer ?))
    =>
    (printout t "Рекомендую послушать песню Beatles, если не понравится советую послушать The doors - light my fire" crlf)
    (assert (answer "Рекомендую послушать песню Beatles, если не понравится советую послушать The doors - light my fire")))

(defrule whatulis-answer2 ""
	(soft no)
	(classic no)
	(whatulis no)
    (Kalinov no)
	(not (answer ?))
	=>
	(printout t "попробуйте послушать песни Калинов Мост - родная :)" crlf)
	(assert (answer "попробуйте послушать песни Калинов Мост - родная :)")))

(defrule maybeFolk-answer2 ""
    (soft no)
    (classic no)
    (whatulis no) ;; Maybe folk
    (Kalinov yes)
    (IDK yes)
    (not (answer ?))
    =>
    (printout t "Круто! Рекомендую послушать еще песню Turn! Turn! Turn!" crlf)
    (assert (answer "Круто! Рекомендую послушать еще песню Turn! Turn! Turn!")))

(defrule IDK-answer2 ""
    (soft no)
    (classic no)
    (whatulis no)
    (Kalinov yes)
    (IDK no)
    (not (answer ?))
    =>
    (printout t "Рекомендую послушать песни The Byrds, если не понравится, можете послушать группу Faith Hill - run" crlf)
    (assert (answer "Рекомендую послушать песни The Byrds, если не понравится, можете послушать группу Faith Hill - run")))

(defrule BTS-answer1 ""
    (soft no)
    (classic no)
    (whatulis no)
    (maybeFolk no)
    ;;(Kalinov no)
    (Kpop yes)
    (BTS yes)
    (not (answer ?))
    =>
    (printout t "Рекомендую так же послушать группу, BLACKPINK - poision" crlf)
    (assert (answer "Рекомендую так же послушать группу, BLACKPINK - poision")))

(defrule BTS-answer2 ""
    (soft no)
    (classic no)
    (whatulis no)
    (maybeFolk no)
    ;;(Kalinov no)
    (Kpop yes)
    (BTS no)
    (not (answer ?))
    =>
    (printout t "Рекомендую послушать песни группы BTS, если не понравится можно так же послушать песни группы - TWICE" crlf)
    (assert (answer "Рекомендую послушать песни группы BTS, если не понравится можно так же послушать песни группы - TWICE")))

(defrule Nothing-answer2 ""
    (soft no)
    (classic no)
    (whatulis no)
    (maybeFolk no)
    (Kalinov no)
    (Kpop no)
    (not (answer ?))
    =>
    (printout t "Жаль что Вы не смогли найти свой любимый жанр(( Рекомендую пройтись по дереву и выбрать случайно песню, уверен Вам понравится" crlf)
    (assert (answer "Жаль что Вы не смогли найти свой любимый жанр(( Рекомендую пройтись по дереву и выбрать случайно песню, уверен Вам понравится")))

;;____________ОТВЕТЫ ЛЕВОЙ ВЕТКИ______________________

(defrule Rammstein-answer1 ""
	;;(future yes)
	(Rammstein yes)
    (RamYes yes) ;; HeldMachine
	(not (answer ?))
	=>
	(printout t "У Вас ОТЛИЧНЫЙ ВКУС!!! Так же рекомендую послушать на подобие группу Sturmann, начать можно с песни Schwarzer Rabe!" crlf)
	(assert (answer "У Вас ОТЛИЧНЫЙ ВКУС!!! Так же рекомендую послушать на подобие группу Sturmann, начать можно с песни Schwarzer Rabe!")))

(defrule Rammstein-answer2 ""
    ;;(future yes)
    (Rammstein yes)
    (RamYes no)
    (not (answer ?))
    =>
    (printout t "Круто что Вы знаете эту группу так же рекомендую послушать Heldmachine - S*XSchlless" crlf)
    (assert (answer "Круто что Вы знаете эту группу так же рекомендую послушать Heldmachine - S*XSchlless")))

(defrule Marlin-answer1 ""
	;;(future yes)
	(Rammstein no)
    (Marlin yes)
	(not (answer ?))
	=>
	(printout t "Отлично! Рекомендую послушать OST+FRONT" crlf)
	(assert (answer "Отлично! Рекомендую послушать OST+FRONT")))

(defrule Marlin-answer2 ""
    (Rammstein no)
    (Marlin no)
    (not (answer ?))
    =>
    (printout t "Возможно Вам понравятся песни этих исполнителей, если нет, могу порекомендовать группу - Linkin Park - Numb" crlf)
    (assert (answer "Возможно Вам понравятся песни этих исполнителей, если нет, могу порекомендовать группу - Linkin Park - Numb")))

(defrule heavyMet-answer1 ""
	(heavyMet no)
    (NotHavy yes) ;; VanHallen
	(not (answer ?))
	=>
	(printout t "Могу посоветовать Вам песню Iron Lounge от группы Radio Head!" crlf)
	(assert (answer "Могу посоветовать Вам песню Iron Lounge от группы Radio Head!")))

(defrule heavyMet-answer2 ""
    (heavyMet no)
    (NotHavy no)
    (not (answer ?))
    =>
    (printout t "Могу посоветовать Вам Motley Crue - Wild Side!" crlf)
    (assert (answer "Могу посоветовать Вам Motley Crue - Wild Side!")))

(defrule Metallica-answer1 "" ;;Heavy Yes
	(Metallica yes)
	(not (answer ?))
	=>
	(printout t "Круто! Могу так же посоветовать послушать группу Nirvana - Smells like teens spirit, уверен понравится." crlf)
	(assert (answer "Круто! Могу так же посоветовать послушать группу Nirvana - Smells like teens spirit, уверен понравится.")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule Metallica-answer2 ""
	(Metallica no)
	(not (answer ?))
	=>
	(printout t "У группы Metallica есть и спокойные и громкие песни, могу порекомендовать Nothing else matter или Master of Puppets соответсвенно, если не понравится можно послушать песни солиста Rammstein Тилля Линдеманна, например песню Ach So Gern." crlf)
	(assert (answer "У группы Metallica есть и спокойные и громкие песни, могу порекомендовать Nothing else matter или Master of Puppets соответсвенно, если не понравится можно послушать песни солиста Rammstein Тилля Линдеманна, например песню Ach So Gern.")))

(defrule Rammstein-answer3 ""
	(pop yes)
	(Rammstein yes)
	(not (answer ?))
	=>
	(printout t "Скорее всего Вы меломан, советую послушать песню от Iggy Pop - In the Death Car" crlf)
	(assert (answer "Скорее всего Вы меломан, советую послушать песню от Iggy Pop - In the Death Car")))

(defrule KatyP-answer1 "" ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (pop yes)
	(KatyP yes)
    (Hanna yes) ;; 5Utra
    ;;;;
	(not (answer ?))
	=>
	(printout t "Могу так же посоветовать Вам послушать песню Фредерико-Феллени, от дуета - Galibri & Mavik" crlf)
	(assert (answer "Могу так же посоветовать Вам послушать песню Фредерико-Феллени, от дуета - Galibri & Mavik")))

(defrule KatyP-answer2 ""
    (pop yes)
   (KatyP yes)
   (Hanna no)
   (not (answer ?))
   =>
    (printout t "Могу посоветовать послушать песню ZEMLYA - EDWXRDX" crlf)
    (assert (answer "Могу посоветовать послушать песню ZEMLYA - EDWXRDX")))

(defrule TaylorS-answer ""
	(TaylorS yes)
	(not (answer ?))
	=>
	(printout t "Супер! Тогда рекомендую послушать песню Заманчивая - Мэтрэнг" crlf)
	(assert (answer "Супер! Тогда рекомендую послушать песню Заманчивая - Мэтрэнг")))


(defrule Mayle-answer1 ""
    (Mayle no) ;; Moya Michel
    (not (answer ?))
    =>
    (printout t "Могу посоветовать послушать Гуляй от Миролюбовой" crlf)
    (assert (answer "Могу посоветовать послушать Гуляй от Миролюбовой")))

(defrule Mayle-answer2 ""
    (Mayle yes) ;; Moya Mishel
    (not (answer ?))
    =>
    (printout t "Круто! Тогда рекомендую Вам послушать - Зари от исполнителся Тони" crlf)
    (assert (answer "Круто! Тогда рекомендую Вам послушать - Зари от исполнителся Тони")))

(defrule electro-answer2 ""
    (electro yes)
    (duftP yes)
    (not (answer ?))
    =>
    (printout t "Круто могу посоветовать песню TR Smooth - Tiga!" crlf)
    (assert (answer "Круто могу посоветовать песню TR Smooth - Tiga!")))

(defrule electro-answer32 ""
	(electro no)
    (Techno no)
	(not (answer ?))
	=>
	(printout t "Жаль, что Вам не удолось найти свой любимый жанр/исполнителя, однако могу порекомендовать Вам песни из альбома игры Atomic Heart, они исполнены в современном стиле, но сами песни времен СССР " crlf)
	(assert (answer "Жаль, что Вам не удолось найти свой любимый жанр/исполнителя, однако могу порекомендовать Вам песни из альбома игры Atomic Heart, они исполнены в современном стиле, но сами песни времен СССР")))

(defrule electro-answer23 ""
    (electro no)
    (Techno yes)
    (not (answer ?))
    =>
    (printout t "Круто! Из техно могу посоветовать The Immortals " crlf)
    (assert (answer "Круто! Из техно могу посоветовать The Immortals")))

(defrule duftP-answer1 ""
	(duftP no)
	(not (answer ?))
	=>
	(printout t "Рекомендую все же послушать песни Duft Punk, начать можно с песни HBFS, или же Get Lucky" crlf)
	(assert (answer "Рекомендую все же послушать песни Duft Punk, начать можно с песни HBFS, или же Get Lucky")))

(defrule duftP-answer2 ""
	(duftP yes)
	(not (answer ?))
	=>
	(printout t "Хороший вкус! Могу так же посоветовать песни Skrillex" crlf)
	(assert (answer "Хороший вкус! Могу так же посоветовать песни Skrillex")))



;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (println crlf "Экспертная система по рекомендации ЛУЧШИХ! песен и исполнителей" crlf))
