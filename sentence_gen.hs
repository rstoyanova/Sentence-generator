import System.Random

example = [["myj", "P", "m"],
          ["govori", "S", "ed"],
          ["golemiyat", "O", "m"],
          ["uchi", "S", "ed"],
          ["jena", "P", "j"],
          ["redovno", "D"],
          ["tyjnata", "O", "j"],
          ["mnogo", "D"],
          ["razbyrkva", "S", "ed"],
          ["testo", "D"],
          ["krasiviyat", "O", "m"],
          ["jena", "P", "j"],
          ["rejem", "S", "mn"],
          ["piper", "D"],
          ["mie", "S", "sr"],
          ["momiche", "P", "sr"],
          ["deteto", "P", "sr"],
          ["siniyat", "O", "m"],
          ["bobkata", "P", "m"],
          ["igrae", "S", "ed"],
          ["v pyasyka", "D"],
          ["na lampata", "D"],
          ["momiche", "P", "sr"],
          ["topcheto", "P", "sr"],
          ["malkoto", "O", "sr"],
          ["navyn", "D"],
          ["razseyanata", "O", "j"],
          ["skacha", "S", "ed"],
          ["stolyt", "P", "m"],
          ["na ploshtadkata", "D"],
          ["gotvi", "S", "ed"],
          ["konche", "P", "sr"],
          ["byaloto", "O", "sr"],
          ["mechka", "P", "j"],
          ["jabata", "P", "j"],
          ["na polyanata", "D"],
          ["pravyat mekici", "S", "mn"],
          ["hora", "P", "mn"],
          ["studenti","P", "mn"],
          ["umnite", "O", "mn"],
          ["golemite", "O", "mn"],
          ["igrae", "S", "ed"],
          ["karti", "D"],
          ["sladkishi", "D"],
          ["hodyat", "S", "mn"]]

--
--getters for the parts of the tuple
--
word tuple = tuple!!0
sent_part tuple = tuple!!1
kind tuple = tuple!!2


--
--gets all word def by part
--
getAll_ part list = filter (\x -> (sent_part x) == part) list


--
--gets the words from the tuples
--
get_words [] = []
get_words (x:xs) = (word x) : (get_words xs)


--
--checks
--
is_upper ch =
        if (((fromEnum ch) >= (fromEnum 'A')) && ((fromEnum ch) <= (fromEnum 'Z')))
                then True
                else False
is_lower ch =
        if (((fromEnum ch) >= (fromEnum 'a')) && ((fromEnum ch) <= (fromEnum 'z')))
                then True
                else False


--
--converts chars
--
toUpperCh ch =
        if is_lower ch
                then toEnum ((fromEnum ch)-32)
                else ch
toLowerCh ch =
        if is_upper ch
                then toEnum ((fromEnum ch)+32)
                else ch


--
--converts strings
--
toUpper str = [toUpperCh x| x <- str]
toLower str = [toLowerCh x| x <- str]


--
--converts string to all lowercase, but the first letter is uppercase
--
firstUpp (x:xs) = (toUpperCh x) : (toLower xs)


--
--adds spaces between each two words
--
addSpaces [] = []
addSpaces [x] = [x]
addSpaces (x:xs) = x : " " : (addSpaces xs)


--
--the final look of the sentence
--
concatAll str = firstUpp (concat (addSpaces str))


--
--extracts the words from a list and concatenates them in a string
--
list_to_sent list = concatAll(get_words list)


--
--length
--
len [] = 0
len list = 1 + len(tail list)

--
--returns the nth element of list
--
get_nth list 1 = head list
get_nth list n = get_nth (tail list) (n - 1)


--
--checks for compatibility
--
check_OP list =
        if ((kind (list!!0)) == (kind (list!!1)))
                then True
                else False

check_PS list
        | kind(list!!1) == "mn" && kind(list!!2) == "mn" = True
        | (kind(list!!1)== "m" || (kind (list!!1))== "j" || (kind (list!!1))== "sr") && kind (list!!2) == "ed" = True
        | otherwise = False

check list =
        if (check_OP list && check_PS list)
                then True
                else False


--
--divides all words in subgroups
--
all_O = getAll_ "O" example
all_P = getAll_ "P" example
all_S = getAll_ "S" example
all_D = getAll_ "D" example


--
--gives a random number in range [1,n]
--
randomMIO :: (Random a) => (a,a) -> IO a
randomMIO range = getStdRandom (randomR range)

randIndx n = randomMIO(1,n)


--
--takes at random words from the four categories and
--if they are compatible returns the sentence
--if not continiues searching
--

getRandomSentence =
        if (check sent)
           then (list_to_sent sent)
           else getRandomSentence
        where    randO = get_nth all_O (randIndx (len all_O))
                 randP = get_nth all_P (randIndx (len all_P))
                 randS = get_nth all_S (randIndx (len all_S))
                 randD = get_nth all_D (randIndx (len all_D))
                 sent = randO : randP : randS : randD : []
