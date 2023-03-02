%! Author:   Vincent Ferrigan <ferrigan@kth.se>
%! Date:     2023-03-02
%! Kurs:     Programmeringsparadigm (progp23)
%! Kurskod:  DD1360
%! Period:   VT23 P3

% Preamble
\documentclass[a4paper, 11pt]{article}
% Packages
\usepackage[T1]{fontenc}
% \usepackage[utf8]{inputenc} % ska den tas bort iom lua?
% \usepackage[utf8]{luainputenc}
\usepackage[swedish]{babel}
\usepackage[a4paper, total={5.5in, 8in}]{geometry}
\usepackage{hyperref}
\usepackage{lmodern}

% Referenser
%\usepackage[
%    backend=biber,
%    % hyperref=true,
%    maxnames=3, 
%    minnames=1, 
%    nohashothers=false
%    bibencoding=utf8, % eventuellt
%    style=apa,
%    % citestyle=apa,
%    pluralothers=true,
%    natbib=true
%    % sorting=nyt
%    % autocite=inline
%    ]{biblatex}
%\DefineBibliographyStrings{english}{andothers={et. al}, and={&}}
%\DeclareLanguageMapping{english}{english-apa}
%\addbibresource{references.bib} % hör till referenser
%

% Haskell Literate Programming
\long\def\ignore#1{}
%% Minted
\usepackage{minted}
\usemintedstyle{colorful}
\newminted[code]{haskell}{}
\newminted[spec]{bash}{}
\setminted[haskell]{
  linenos=true,
  frame=single,
  fontsize=\footnotesize,
  }
\setminted[bash]{
%  label=Exempelkörning i ghci,
  frame=single,
  fontsize=\footnotesize,
  }

%% listings
%\usepackage{listings}
%\lstloadlanguages{Haskell}
%\lstnewenvironment{code}
%    {\lstset{}%
%      \csname lst@SetFirstLabel\endcsname}
%    {\csname lst@SaveFirstLabel\endcsname}
%    \lstset{
%      basicstyle=\small\ttfamily,
%      flexiblecolumns=false,
%      basewidth={0.5em,0.45em},
%      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
%               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
%               {\\\\}{{\char`\\\char`\\}}1
%               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
%               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
%               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
%               {>>}{{>>}}2 {>>=}{{>>=}}2
%               {|}{{$\mid$}}1               
%    }
% 

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
\section{En lista med heltal}
Funktionen \mintinline{haskell}{squarePositive} räknas som en
\emph{sammansatt funktion} (eng. \emph{function composition}) som här bildats
genom att sätta samma två funktioner av \emph{högre ordning} (eng. \emph{high-order functions}).
%    \begin{figure}[H]
\begin{code}
-- | Square all positive integers in a list.
-- Ignore and exclude both negative numbers and zero from the list.
squarePositive :: [Int] -> [Int]
squarePositive = map (^2) . filter (>0) 
\end{code}
%    \label{code:squarePositive}
%    \end{figure}
I grova drag så filtrerar den inre funktionen, \mintinline{haskell}{filter}, 
bort de element som inte ska kvadreras 
medan den yttre funktionen, \mintinline{haskell}{map}, kvadrerar dem.
Med andra ord så applicerar \mintinline{haskell}{map} och \mintinline{haskell}{filter}
varsin givna funktion på varje element i en given lista.

\subsection{Högre ordningens funktioner}
Det som gör \emph{map} och \emph{filter} till högre ordningens funktioner 
är att de kan ta en funktion som parameter. 
Här tar båda emot \emph{partiellt applicerade funktioner} av \emph{binära operatorer} 
-- som även kallas för \emph{sections} eller \emph{operator sections}
(REF!! s 44, hutton och %https://www.haskell.org/onlinereport/exps.html#sections
). 
Paramentern för \emph{operator sections} samt parametern för den sammansatta funktionen 
är underförstådd (eng. \emph{tacit}) och är skriven i s.k. \emph{point-free style}. 
Vilket är ett sätt att skriva inom \emph{Tacit Programming}.

I lösningsförslaget returnerar
\mintinline{haskell}{filter (>0)} en ny lista innehållande endast de element som uppfyller 
vilkoret \mintinline{haskell}{>0}. 
Vilkoret är ett \emph{predikat} 
dvs en funktion med returtyp \emph{Boolean}. 
\mintinline{haskell}{map (^2)} applicerar därefter \mintinline{haskell}{(^2)} 
på alla element och skapar en ny lista med resultaten. 

Operatorn \mintinline{haskell}{(.)} returnerar 
en sammansättning av funktioner som en enskild funktion.
%(**REF s81 hutton) 
Vilket medför att även den tillhör högre ordningens funktioner. 

%parencite{}
%textcite{}
%Vilket innebär att en har redan angett ett av argumenten i en infix operator. 

\subsection{Immutability}
Data i Haskell är icke-muterbar (eng. \emph{immutable}. 
När \emph{map} används, skapas därför en ny lista med nya värden. 
Dessa värden baseras på resultatet av att applicera den givna funktions-argumentet på den mottagna listan.
Funktions-argumentet \mintinline{haskell}{(^2)} är en kvadreringsfunktion skriven som \emph{operator section}
-- ett syntaktiskt socker vars lambda uttryck är \mintinline{haskell}{\x -> x^2}).

Även om filter-funktionen returnerar en ny lista och ''\emph{immutability}'' råder,
så innebär det inte att den nya listan innehåller kopior,
utan snarare så pekar den nya listan enbart på de elements som uppfyller vilkoret i den givna 
predikat-argumentet.

\subsection{Exempelkörning}
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
%    \label{code:ghciTask1}
%    \end{figure}

\clearpage
\section{Resmål}
Likt uppgift 1 så har funktionen i denna uppgift, \mintinline{haskell}{jointDestinations}, 
implementerats som en \emph{sammansatt funktion}.
Även här är funktionen skriven i \emph{pointfree style}.%, vilket innebär att den sammansatta funktionen definieras utan parameter
%    \begin{figure}[H]
\begin{code}
type Destination = [Char]

data Person  = Person
    { name         :: [Char] 
    , age          :: Int
    , destinations :: [Destination]
    } deriving (Show, Eq)

jointDestinations :: [Person] -> [Destination]
jointDestinations = map head . List.group . List.sort . concatMap destinations
\end{code}
%    \label{code:resmal}
%    \end{figure}
Ovanstående funktion tar emot en lista av typ \mintinline{haskell}{Person}
och returnerar en lista av typ \mintinline{haskell}{Destination}
(se exempelkörningarna i \emph{ghci} nedan).
\mintinline{haskell}{type Destination} är ett alias 
för data-typen \mintinline{haskell}{[Char]}. 
Med andra ord så returnerar funktionen \mintinline{haskell}{jointDestinations} 
en lista av strängar som här representerar 
alla de resmål som den mottagna listans element har gemensamt.
%TODO: Här kan du lägga till förklaringen på concatMap, sort, group map och head.
%Hör om det är ett krav.

I Haskell kan ADT av produkt typ defineras med s.k. \emph{Record-syntax}.
I lösningsförslaget har denna syntax används för data typen \mintinline{haskell}{Person}.
Denna ADT (eller record) har \mintinline{haskell}{Person} som konstruktor.
Konstruktorn kan ta emot tre parametrar för att skapa en record; 
\mintinline{haskell}{name} av typen \mintinline{haskell}{[Char]} (dvs en sträng), 
\mintinline{haskell}{age} av typen \mintinline{haskell}{Int} och
\mintinline{haskell}{destinations} av typen \mintinline{haskell}{[Destination]}. 
Det sistnämnda fältet är, som tidigare nämnts, en lista av strängar.

Med hjälp av konstruktorn kan instanser av \mintinline{haskell}{Person} skapas på följande sätt:
\begin{spec}
ghci> :set +m
ghci> let vincent = Person "Vincent Ferrigan" 
ghci|               42  ["Sweden", "Norway", "Poland"]
ghci| 
ghci> vincent
Person {name = "Vincent Ferrigan", age = 42, destinations = ["Sweden","Norway",
"Poland"]}
ghci> let pontus = Person { age=42, 
ghci|                     , destinations=["Sweden", "Norway", "Germany"]
ghci|                     , name="Pontus Pontusson"}
ghci> let david = Person { destinations=["Sweden", "Spain", "Portugal"]
ghci|                      , age=22
ghci|                      , name="David Davidsson"}
ghci| 
ghci> david
Person {name = "David Davidsson", age = 22, destinations = ["Sweden","Spain",
"Portugal"]}
\end{spec}
Antingen likt den instansiering som gäller för andra ADT:n, 
som produkt och summerings typer, eller
enligt Records syntax, där ordning inte spelar roll. 

Haskell skapar även automatisk funktioner i form av \emph{getters}. 
Se exempelkörningen nedan:
\begin{spec}
ghci> destinations vincent
["Sweden","Norway","Poland"]
ghci> david
Person {name = "David Davidsson", age = 22, destinations = ["Sweden","Spain","Portugal"]}
ghci> age pontus
42
\end{spec}
Appliceras funktion \mintinline{haskell}{jointDestinations} erhålls följande:
\begin{spec}
ghci> jointDestinations [pontus, david, vincent]
["Denmark","Germany","Norway","Portugal","Spain","Sweden"]
\end{spec}

\section{Svansrekursiv sifferkedja}

%    \begin{figure}[H]
\begin{code}
numberChain :: Int -> Int -> [Int]
numberChain x y = reverse $ numberChain' (x:[]) y
  where
    numberChain' :: [Int] -> Int -> [Int]
    numberChain' ys@(x:xs) y 
      | x == y    = ys
      | x < y     = numberChain' (x+2:ys) y
      | otherwise = numberChain' (x-3:ys) y
\end{code}
%    \label{code:numberChain}
%    \end{figure}

\clearpage
\section{Kortaste vägen}

%    \begin{figure}[H]
\begin{code}
type Point    = (Double, Double)
type Distance = Double

square :: Num a => a -> a
square x = x * x

-- distance :: Floating a => (a, a) -> (a, a) -> a
distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) = sqrt $ square x' + square y'
 where
    x' = abs $ x2 - x1
    y' = abs $ y2 - y1

-- totalDistance :: Floating a => [(a, a)] -> a
totalDistance :: [Point] -> Distance
totalDistance [] = 0
totalDistance ps@(_:ps') = sum $ zipWith distance ps ps'
\end{code}
%    \label{code:totalDistance}
%    \end{figure}

\end{document}
