%! Author:   Vincent Ferrigan <ferrigan@kth.se>
%! Date:     2023-03-02
%! Course:   Programmeringsparadigm (progp23)
%! Kurskod:  DD1360
%! Period:   VT23 P3

% Haskell Literate Programming
\long\def\ignore#1{}

% Module header
  \ignore{
\begin{code}
{- |
File name      : MpFP.lhs
Module         : MpFP
Description    : Mästarprov Funktionell Programmering
                 <https://canvas.kth.se/courses/38058/assignments/237975>
Course         : DD1360 VT23 Programmeringsparadigm (progp23)
Author/Student : Vincent Ferrigan
maintainer     : ferrigan@kth.se
-}

module MpFP 
    ( -- * Data type
      Person             -- TASK 2
    , Destination
      -- * Functions
    , squarePositive     -- TASK 1
    , jointDestinations  -- TASK 2
    , numberChain        -- TASK 3
    , totalDistance      -- TASK 4
    ) where

import qualified Data.List as List
import GHC.Arr (listArray)
import GHC.Read (list)
\end{code}
  }

% Preamble
\documentclass[a4paper, 11pt]{article}
% Packages
\usepackage[T1]{fontenc}
% \usepackage[utf8]{inputenc} % ska den tas bort iom lua?
% \usepackage[utf8]{luainputenc}
\usepackage[swedish]{babel}
\usepackage[a4paper, total={5.5in, 8.6in}]{geometry}
\usepackage{hyperref}
\usepackage{lmodern}

% Haskell Literate Programming
\long\def\ignore#1{} % För kodsnuttar som ska ignoreras av tex
%% Minted
\usepackage{minted}
\usemintedstyle{colorful}
\newminted[code]{haskell}{}
\newminted[spec]{text}{}
% \newminted[spec]{bash}{}
\setminted[haskell]{
  linenos=true,
  frame=single,
  fontsize=\footnotesize,
  }
\setminted[text]{
%  label=Exempelkörning i ghci,
  frame=single,
  %fontsize=\footnotesize,
  fontsize=\scriptsize,
  }

\title{Mästarprov Funktionell Programmering
%\\Funktionell Programmering
\\ \small{DD1360 Programmeringsparadigm (progp23), VT23}}

\author{Vincent Ferrigan 
\\ \href{mailto:ferrigan@kth.se}{ferrigan@kth.se} 
\\ \small{19810222-0558}}

\date{\today}

% Document
\begin{document}
  \maketitle
  \clearpage
  
\section*{Litterärt program}
Dokumentet är skriven som ett litterärt program i Haskell.
Haskell har ett inbyggt programmeringsstöd för litterär programmering 
(eng. \emph{literate programming}) där  
källkoden för det ''litterära programmet'' har suffixet \mintinline{bash}{.lhs} 
(som står för \emph{Literate Haskell Script}).
Källkoden kan kompileras till körbar kod i \emph{GHC} och till pdf med exemplevis, som i detta fall, 
Lua\LaTeX.

Programmet är skriven som ett biblioteksmodul som 
kan laddas in i \emph{GHCi} för testkörning.
Programmet har även enhetstestats 
(eng. \emph{unit tested}) i \emph{Cabal} med hjälp av testbiblioteket \emph{HUnit}.

All Haskellkod för GHC-kompilatorn är omslagen i \LaTeX-stil 
istället för att skrivas med \emph{bird style}. 

\clearpage
\section*{Uppgift 1 -- En lista med heltal}
\label{sec:1}
\subsection*{Lösningsförslag}
%    \begin{figure}[H]
\begin{code}
-- | Square all positive integers in a list.
-- Ignore and exclude both negative numbers and zero from the list.
squarePositive :: [Int] -> [Int]
squarePositive = map (^2) . filter (>0) 
\end{code}
%    \label{code:squarePositive}
%    \end{figure}
\subsection*{Sammansatt funktion}
Funktionen \mintinline{haskell}{squarePositive} räknas som en
\emph{sammansatt funktion} (eng. \emph{function composition}) som här bildats
genom att sätta samma två funktioner av \emph{högre ordning} (eng. \emph{high-order functions}).
I grova drag så filtrerar den inre funktionen, \mintinline{haskell}{filter}, 
bort de element som inte ska kvadreras 
medan den yttre funktionen, \mintinline{haskell}{map}, kvadrerar dem.
Med andra ord så applicerar \mintinline{haskell}{map} och \mintinline{haskell}{filter}
varsin givna funktion på varje element i en given lista. 
En mer genomgående förklaring ges nedan.

\subsection*{Högre ordningens funktioner}
Det som gör \emph{map} och \emph{filter} till högre ordningens funktioner 
är att de kan ta en funktion som parameter. 
Här tar båda emot \emph{partiellt applicerade funktioner} av \emph{binära operatorer} 
-- som även kallas för \emph{sections} eller \emph{operator sections}.
%(REF!! s 44, hutton och https://www.haskell.org/onlinereport/exps.html#sections). 
Parametern för \emph{operator sections} samt parametern för den sammansatta funktionen 
är underförstådd (eng. \emph{tacit}) och är skriven i s.k. \emph{point-free style}. 
Vilket är ett sätt att skriva inom \emph{Tacit Programming}.

I lösningsförslaget returnerar
\mintinline{haskell}{filter (>0)} en ny lista innehållande endast de element som uppfyller 
villkoret \mintinline{haskell}{>0}. 
Villkoret är ett \emph{predikat} 
dvs. en funktion med returtyp \emph{Boolean}. 
\mintinline{haskell}{map (^2)} applicerar därefter \mintinline{haskell}{(^2)} 
på alla element och skapar en ny lista med resultaten. 

Då operatorn \mintinline{haskell}{(.)} returnerar 
en sammansättning av funktioner som en enskild funktion räknas även den till 
högre ordningens funktioner. 
%(**REF s81 hutton) 

\subsection*{Immutability}
Data i Haskell är icke-muterbar (eng. \emph{immutable}). 
När \emph{map-funktionen} används, skapas därför en ny lista med nya värden. 
Dessa värden baseras på resultatet av att applicera den givna funktions-argumentet på den mottagna listan.
Funktions-argumentet \mintinline{haskell}{(^2)} är en kvadreringsfunktion skriven som \emph{operator section}
-- ett syntaktiskt socker vars lambda uttryck är \mintinline{haskell}{\x -> x^2}.
% Alternativt lambdanotation

Även om \emph{filter-funktionen} returnerar en ny lista och ''\emph{immutability}'' råder,
så innebär det inte per se att den nya listan innehåller kopior,
utan snarare så pekar den nya listan enbart på de elements som uppfyller villkoret i den givna 
predikat-argumentet.

\subsection*{Exempelkörning}
%    \begin{figure}[H]
\begin{spec}
ghci> :l src/MpFP.lhs
[1 of 1] Compiling Main        ( src/MpFP.lhs, interpreted )
Ok, one module loaded.
ghci> sq
sqrt     squarePositive
ghci> squarePositive [-2, -1, 0, 1, 2]
[1,4]
ghci> squarePositive [1, 3, 2]
[1,9,4]
ghci> squarePositive [0, 0, 0, 0]
[]
ghci> squarePositive [-3, -5, -8, 2]
[4]
\end{spec}
%    \label{ghci:Task1}
%    \end{figure}

\clearpage
\section*{Uppgift 2 -- Resmål}
\subsection*{Lösningsförslag}
%    \begin{figure}[H]
\begin{code}
type Destination = [Char]

data Person  = Person
    { name         :: [Char] 
    , age          :: Int
    , destinations :: [Destination]
    } deriving (Show, Eq)

-- | Return a list of joint 'destinations' from a list of Persons, 
-- only including unique elements. 
jointDestinations :: [Person] -> [Destination]
jointDestinations = map head . List.group . List.sort . concatMap destinations
\end{code}
%    \label{code:resmal}
%    \end{figure}
Likt uppgift 1 så har funktionen i denna uppgift, \mintinline{haskell}{jointDestinations}, 
implementerats som en \emph{sammansatt funktion}.
Även här är funktionen skriven i \emph{pointfree style}.%, vilket innebär att den sammansatta funktionen definieras utan parameter
Ovanstående funktion tar emot en lista av typ \mintinline{haskell}{Person}
och returnerar en lista av typ \mintinline{haskell}{Destination}
(se exempelkörningarna i \emph{GHCi} nedan).
\mintinline{haskell}{type Destination} är ett alias 
för data-typen \mintinline{haskell}{[Char]}. 
Med andra ord så returnerar funktionen \mintinline{haskell}{jointDestinations} 
en lista av strängar som här representerar 
alla de resmål som den mottagna listans element har gemensamt.
%TODO: Här kan du lägga till förklaringen på concatMap, sort, group map och head.
%Hör om det är ett krav.

\subsection*{Record-syntax}
I Haskell kan ADT (\emph{Abstract Data Type}) av produkt typ definieras med s.k. \emph{Record-syntax}.
I lösningsförslaget har denna syntax används för data typen \mintinline{haskell}{Person}.
Denna ADT (eller record) har \mintinline{haskell}{Person} som konstruktor.
Konstruktorn kan ta emot tre parametrar för att skapa en record; 
\mintinline{haskell}{name} av typen \mintinline{haskell}{[Char]} (dvs. en sträng), 
\mintinline{haskell}{age} av typen \mintinline{haskell}{Int} och
\mintinline{haskell}{destinations} av typen \mintinline{haskell}{[Destination]}. 
Det sistnämnda fältet är, som tidigare nämnts, en lista av strängar. 

\subsection*{Instansiering}
Med hjälp av konstruktorn kan instanser av \mintinline{haskell}{Person} skapas 
antingen likt den instansiering som gäller för andra ADT:n (som produkt och summerings typer) 
eller enligt Record-syntax, där ordning inte spelar någon roll. 
See nedansteånde exempelkörning.
\clearpage
\subsection*{Exempelkörning}
\begin{spec}
ghci> :set +m
ghci> let vincent = Person "Vincent Ferrigan" 42 ["Sweden", "Norway", "Poland"]
ghci> vincent
Person {name = "Vincent Ferrigan", age = 42, destinations = ["Sweden","Norway","Poland"]}
ghci> let pontus = Person { age=42 , destinations=["Sweden", "Norway", "Germany"]
ghci|                     , name="Pontus Pontusson"}
ghci> let david = Person { destinations=["Sweden", "Spain", "Portugal"]
ghci|                      , age=22, name="David Davidsson"}
ghci| 
ghci> david
Person {name = "David Davidsson", age = 22, destinations = ["Sweden","Spain","Portugal"]}
\end{spec}
Appliceras funktionen \mintinline{haskell}{jointDestinations} på en lista över 
ovansteånde instanser erhålls följande:
\begin{spec}
ghci> jointDestinations [pontus, david, vincent]
["Denmark","Germany","Norway","Portugal","Spain","Sweden"]
\end{spec}
Haskell skapar även automatisk funktioner i form av \emph{getters} med 
samma namn som recordfältnamnen. 
Se exempelkörningen nedan:
\begin{spec}
ghci> destinations vincent
["Sweden","Norway","Poland"]
ghci> name david
"David Davidsson"
ghci> age pontus
42
\end{spec}

\clearpage
\section*{Uppgift 3 -- Svansrekursiv sifferkedja}
\subsection*{Lösningsförslag}
%TODO ska 'current' anv istället för x och 'goal' istället för 'y'. Se uppgiftsbeskrivning
%    \begin{figure}[H]
\begin{code}
-- | Return a sequence of numbers from range x to y as a list of integers.
-- Steps: Increment by 2 but, if out of range, decrement by 3.
numberChain :: (Integral a) => a -> a -> [a]
-- numberChain :: Int -> Int -> [Int]
numberChain x y = reverse $ numberChain' [x] y
  where
    numberChain' :: (Integral a) => [a] -> a -> [a]
    -- numberChain' :: [Int] -> Int -> [Int]
    numberChain' ys@(x:xs) y 
      | x == y    = ys
      | x < y     = numberChain' (x+2:ys) y
      | otherwise = numberChain' (x-3:ys) y
\end{code}
Huvudfunktionen \mintinline{haskell}{numberChain} returnerar resultatet
från hjälpfunktionen i omvänd ordning. Detta med hjälp av den 
inbyggda funktionen \mintinline{haskell}{reverse}.
Notera att \mintinline{haskell}{reverse} inte kopierar elementen från den givna listan,
utan snarare konstruerar en ny länkad lista med en ny mängd pekare. 
Dessa pekar på samma element som den tidigare listan.  
Traverseringen sker därmed bara en gång och används i huvudfunktionen för att 
undvika \mintinline{haskell}{(++)} operatorn i den rekursiva hjälpfunktionen. 
\subsection*{Svansrekursion}
Hjälpfunktionen \mintinline{haskell}{numberChain'} är i sin tur svansrekursiv
då det rekursiva anropet står/utförs sist i funktionen och lämnar inga 
operationer ''hängande'' när det rekursiva anropet utförs.
Med andra ord är resultatet av det rekursiva anropet inte en operand till en operation.
Det som gör själva anropet rekursivt, är att hjälpfunktionen anropar sig självt. 
\subsection*{Exempelkörning}
%    \begin{figure}[H]
\begin{spec}
ghci> numberChain 3 16
[3, 5, 7, 9, 11, 13, 15, 17, 14, 16]
ghci> numberChain 1 2
[1, 3, 0, 2]
ghci> numberChain 1 1
[1]
ghci> numberChain 17 42
[17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 40, 42]
ghci> numberChain (-1) (-10)
[-1, -4, -7, -10]
ghci> numberChain (-1) (-9)
[-1, -4, -7, -10, -8, -11, -9]
\end{spec}
%    \label{ghci:numberChain}
%    \end{figure}

\clearpage
\section*{Uppgift 4 -- Kortaste vägen}
\subsection*{Lösningsförslag}
%    \begin{figure}[H]
\begin{code}
type Point    = (Double, Double)
type Distance = Double

-- | Square given number
square :: Num a => a -> a
square x = x * x

-- | Calculate distance between two coordinate points using Pythagorean theorem.
distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) = sqrt $ square x' + square y'
 where
    x' = abs $ x2 - x1
    y' = abs $ y2 - y1

-- Return the sum of distances between the points in given list order. 
totalDistance :: [Point] -> Distance
totalDistance [] = 0
totalDistance ps@(_:ps') = sum $ zipWith distance ps ps'
\end{code}
%    \label{code:totalDistance}
%    \end{figure}
\subsection*{Referenstransparens}
Hjälpfunktionerna \mintinline{haskell}{distance}, \mintinline{haskell}{square}, 
\mintinline{haskell}{x'} och \mintinline{haskell}{y'}
fyller ingen annan funktion än att beräkna ett värde. 
Det innebär att varje anrop till dem kan bytas ut mot dess funktionskropp, 
där parametrarna ersätts med motsvarande argument. 
Dessa funktioner är med andra ord uttryck och inte satser 
(eng. \emph{functions are expressions and not statements}) som kan ersättas med sitt värde. 
Vilket innebär att uttrycken är referenstransparenta. 

\subsection*{Äkta funktioner}
Likt hjälpfunktionerna räknas funktionen \mintinline{haskell}{totalDistance} 
som \emph{äkta funktioner} (eng. \emph{pure functions}). 
Dels för att de saknar sidoeffekter och 
dels för att de alltid ger samma resultat om
de anropas samma värde. 
Resultatet beror med andra ord enbart på dess argument.

\subsection*{Exempelkörning}
%    \begin{figure}[H]
\begin{spec}
ghci> totalDistance [(1.3, 2.4), (5.3, -1.3), (-4.2, -3.4), (5.2, 8.0)]
29.953845823305446
ghci> totalDistance [(1.3, 2.4), (5.3, -1.3)]
5.448853090330111
ghci> totalDistance [(5.3, -1.3), (-4.2, -3.4)]
9.729337079164232
ghci> totalDistance [(-4.2, -3.4), (5.2, 8.0)]
14.775655653811103
ghci> totalDistance []
0.0
ghci> totalDistance [(3.24, 14.23)]
0.0
\end{spec}
\end{document}
