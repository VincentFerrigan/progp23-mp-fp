# Mästarprov Funktionell Programmering
# progp23-mp-fp

## Litterärt program
Dokumentet är skriven som ett litterärt program i Haskell.
Haskell har ett inbyggt programmeringsstöd för litterär programmering
(eng. *literate programming*) där  
källkoden för det ''litterära programmet'' har suffixet ` .lhs`
(som står för *Literate Haskell Script*).
Källkoden kan kompileras till körbar kod i *GHC* och till pdf med exempelvis, som i detta fall,
Lua\LaTeX.

Programmet är skriven som ett biblioteksmodul som
kan laddas in i *GHCi* för testkörning.
Programmet har även enhetstestats
(eng. *unit tested*) i *Cabal* med hjälp av testbiblioteket *HUnit*.

All Haskellkod för GHC-kompilatorn är omslagen i LaTeX-stil
istället för att skrivas med *bird style*.

## 1. En lista med heltal
Implementera en funktion i Haskell som tar in en lista av heltal och returnerar
en lista där varje element är kvadraten av det ursprungliga elementet, förutom
de element som är negativa eller 0. Icke-positiva element ska istället
ignoreras och inte finnas med i den resulterande listan.
**Exempelkörningar:**
```bash
prelude> :l list_of_numbers.hs
[1 of 1] Compiling Main        ( list_of_numbers.hs, interpreted )
Ok, one module loaded.
*Main> sq
sqrt     squarePositive
*Main> squarePositive [-2, -1, 0, 1, 2]
[1,4]
*Main> squarePositive [1, 3, 2]
[1, 9, 4]
*Main> squarePositive [0, 0, 0, 0]
[]
*Main> squarePositive [-3, -5, -8, 2]
[4]
```

Implementationen av squarePositive behöver uppfylla följande krav:

1. Du behöver använda högre ordningens funktioner för att implementera
   funktionen. 
   
Dokumentationen av squarePositive behöver uppfylla följande krav:

1. Redogör för vilka högre ordningens funktioner som du använder och förklara
   varför dessa är högre ordningens funktioner.
2. Redogör för hur funktionen använder immutability.

### Lösningsförslag

```Haskell
-- | Square all positive integers in a list.
-- Ignore and exclude both negative numbers and zero from the list.
squarePositive :: [Int] -> [Int]
squarePositive = map (^2) . filter (>0)
```

#### Sammansatt funktion
Funktionen `squarePositive` räknas som en
*sammansatt funktion* (eng. *function composition*) som här bildats
genom att sätta samma två funktioner av *högre ordning* (eng. *high-order functions*).
I grova drag så filtrerar den inre funktionen, `filter`,
bort de element som inte ska kvadreras
medan den yttre funktionen, `map`, kvadrerar dem.
Med andra ord så applicerar `map` och `filter`
varsin givna funktion på varje element i en given lista.
En mer genomgående förklaring ges nedan.

#### Högre ordningens funktioner
Det som gör `map` och `filter` till högre ordningens funktioner
är att de kan ta en funktion som parameter.
Här tar båda emot *partiellt applicerade funktioner* av *binära operatorer*
-- som även kallas för *sections* eller *operator sections*.
Parametern för *operator sections* samt parametern för den sammansatta funktionen
är underförstådd (eng. *tacit*) och är skriven i s.k. *point-free style*.
Vilket är ett sätt att skriva inom *Tacit Programming*.

I lösningsförslaget returnerar
`filter (>0)` en ny lista innehållande endast de element som uppfyller
villkoret `>0`.
Villkoret är ett *predikat*
dvs. en funktion med returtyp `Boolean`.
`map (^2)` applicerar därefter `(^2)`
på alla element och skapar en ny lista med resultaten.

Då operatorn `(.)` returnerar
en sammansättning av funktioner som en enskild funktion räknas även den till
högre ordningens funktioner.

#### Immutability
Data i Haskell är icke-muterbar (eng. *immutable*).
När *map-funktionen* används, skapas därför en ny lista med nya värden.
Dessa värden baseras på resultatet av att applicera den givna funktions-argumentet på den mottagna listan.
Funktions-argumentet `(^2)` är en kvadreringsfunktion skriven som \emph{operator section}
-- ett syntaktiskt socker vars lambda uttryck är `\x -> x^2`.
% Alternativt lambdanotation

Även om *filter-funktionen* returnerar en ny lista och ''*immutability*'' råder,
så innebär det inte per se att den nya listan innehåller kopior,
utan snarare så pekar den nya listan enbart på de elements som uppfyller villkoret i den givna
predikat-argumentet.

#### Exempelkörning
```bash
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
```


## 2. Resmål
Definiera ett *Record* för enskilda personer kallat *Person* som har namn, ålder
och en lista över strängar som representerar resmål. 

Skriv sedan en funktion som läser in en lista över Person och returnerar en
lista över alla resmål som de tillsammans har besökt med alla resmål som
besökts av minst en person i listan. 

Kravet på listan är att alla resmål ska vara unika.
Uppgiften går att lösa effektivt men det är inget krav. En lösning som är
O(n^3) ger godkänt.

**Dokumentationskrav:** Dokumentera utförligt hur man skapar en instans av Person.

Ledning: Learn You a Haskell har en [guide till hur du använder Record Syntax](http://learnyouahaskell.com/making-our-own-types-and-typeclasses).

### Lösningsförslag}
```haskell
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
```

Likt uppgift 1 så har funktionen i denna uppgift, `jointDestinations`,
implementerats som en \emph*sammansatt funktion*.
Även här är funktionen skriven i *pointfree style*., vilket innebär att den sammansatta funktionen definieras utan parameter
Ovanstående funktion tar emot en lista av typ `Person`
och returnerar en lista av typ `Destination`
(se exempelkörningarna i *GHCi* nedan).
`type Destination` är ett alias
för data-typen `[Char]`.
Med andra ord så returnerar funktionen `jointDestinations`
en lista av strängar som här representerar
alla de resmål som den mottagna listans element har gemensamt.

#### Record-syntax
I Haskell kan ADT (*Abstract Data Type*) av produkt typ definieras med s.k. *Record-syntax*.
I lösningsförslaget har denna syntax används för data typen `Person`.
Denna ADT (eller record) har `Person` som konstruktor.
Konstruktorn kan ta emot tre parametrar för att skapa en record;
`name` av typen `[Char]` (dvs. en sträng),
`age` av typen `Int` och
`destinations` av typen `[Destination]`.
Det sistnämnda fältet är, som tidigare nämnts, en lista av strängar.

#### Instansiering
Med hjälp av konstruktorn kan instanser av `Person` skapas
antingen likt den instansiering som gäller för andra ADT:n (som produkt och summerings typer)
eller enligt Record-syntax, där ordning inte spelar någon roll.
See nedanstående exempelkörning.
\clearpage
#### Exempelkörning}
```bash
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
```
Appliceras funktionen `jointDestinations` på en lista över
ovansteånde instanser erhålls följande:
```haskell
ghci> jointDestinations [pontus, david, vincent]
["Denmark","Germany","Norway","Portugal","Spain","Sweden"]
```
Haskell skapar även automatisk funktioner i form av \emph*getters* med
samma namn som recordfältnamnen.
Se exempelkörningen nedan:
```bash
ghci> destinations vincent
["Sweden","Norway","Poland"]
ghci> name david
"David Davidsson"
ghci> age pontus
42
```

## 3. Svansrekursiv sifferkedja
En sifferlek går till på följande sätt. Du börjar på ett tal current och ska nå
ett tal goal. Dina två tillåtna drag är:
1. Att lägga till 2 till talet
2. Att dra ifrån 3 från talet.

Leken tar slut då du når goal. Leken ställer krav på att du loggar alla
delresultat på vägen. Det finns alltid minst en giltig lösning.
**Exempelkörningar:**
```
*Main> :l numberChain.hs
[1 of 1] Compiling Main        ( numberChain.hs, interpreted )
Ok, one module loaded.
*Main> numberChain 3 16
[3, 5, 7, 9, 11, 13, 15, 17, 14, 16]
*Main> numberChain 1 2
[1, 3, 0, 2]
*Main> numberChain 1 1
[1]
*Main> numberChain 17 42
[17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 40, 42]
```
Du behöver också kunna hantera negativa tal. Gör detta genom att skriva talet
inom parentes så att Haskell förstår att det är unärt minus.
```
*Main> numberChain (-1) (-10)
[-1, -4, -7, -10]
*Main> numberChain (-1) (-9)
[-1, -4, -7, -10, -8, -11, -9]
```

**Ledning:**

1. En girig algoritm som alltid går mot målet när den kan och sedan antingen
   backar eller skjuter över målet ger en optimal lösning.

**Krav på implementationen:**

1. Du behöver använda en hjälpfunktion.
2. Högre ordningens funktioner är förbjudna här.
3. Hjälpfunktionen behöver vara implementerad med hjälp av svansrekursion.
4. Din lösning får inte använda slump.

**Krav på dokumentationen:**
1. Skriv vad som gör din implementation svansrekursiv.

### Lösningsförslag
```haskell
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
```
Huvudfunktionen `numberChain` returnerar resultatet
från hjälpfunktionen i omvänd ordning. Detta med hjälp av den
inbyggda funktionen `reverse`.
Notera att `reverse` inte kopierar elementen från den givna listan,
utan snarare konstruerar en ny länkad lista med en ny mängd pekare.
Dessa pekar på samma element som den tidigare listan.  
Traverseringen sker därmed bara en gång och används i huvudfunktionen för att
undvika `(++)` operatorn i den rekursiva hjälpfunktionen.
\subsection*{Svansrekursion}
Hjälpfunktionen `numberChain'` är i sin tur svansrekursiv
då det rekursiva anropet står/utförs sist i funktionen och lämnar inga
operationer ''hängande'' när det rekursiva anropet utförs.
Med andra ord är resultatet av det rekursiva anropet inte en operand till en operation.
Det som gör själva anropet rekursivt, är att hjälpfunktionen anropar sig självt.
####
```bash
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
```


## 4. Kortaste vägen
Skriv en funktion shortestpath som tar in en lista med koordinater i planet och
beräknar den kortaste vägen som går igenom alla punkter i den ordning som de
var givna i listan.
**Exempelkörning:**
```
*Main> totalDistance [(1.3, 2.4), (5.3, -1.3), (-4.2, -3.4), (5.2, 8.0)]
29.953845823305446
*Main> totalDistance [(1.3, 2.4), (5.3, -1.3)]
5.448853090330111
*Main> totalDistance [(5.3, -1.3), (-4.2, -3.4)]
9.729337079164232
*Main> totalDistance [(-4.2, -3.4), (5.2, 8.0)]
14.775655653811103
```

Tänk också på dessa specialfall:
```
*Main> totalDistance []
0.0
*Main> totalDistance [(3.24, 14.23)]
0.0
```

**Ledning:**
1. Avståndet mellan två punkter kan beräknas med Pythagoras sats.
2. Testa med exemplet ovan, som ges i lättpastead text här: 
   [(1.3, 2.4), (5.3, -1.3), (-4.2, -3.4), (5.2, 8.0)]

**Dokumentations- och implementationskrav:**
1. Impementera uppgiften med rena funktioner och dokumentera vad som gör dem rena.
2. Implementera uppgiften med referenstransparens och dokumentera vad som gör
   koden referenstransparent.

### Lösningsförslag
```haskell
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
```

#### Referenstransparens
Hjälpfunktionerna `distance`, `square`,
`x'` och `y'`
fyller ingen annan funktion än att beräkna ett värde.
Det innebär att varje anrop till dem kan bytas ut mot dess funktionskropp,
där parametrarna ersätts med motsvarande argument.
Dessa funktioner är med andra ord uttryck och inte satser
(eng. *functions are expressions and not statements*) som kan ersättas med sitt värde.
Vilket innebär att uttrycken är referenstransparenta.

#### Äkta funktioner}
Likt hjälpfunktionerna räknas funktionen `totalDistance`
som *äkta funktioner* (eng. *pure functions*).
Dels för att de saknar sidoeffekter och
dels för att de alltid ger samma resultat om
de anropas samma värde.
Resultatet beror med andra ord enbart på dess argument.

#### Exempelkörning
```bash
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
```