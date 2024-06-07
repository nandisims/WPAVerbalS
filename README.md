This repository includes the data (verbalS.csv) and statistical analysis code (verbalS.R) associated with:
Sims, Nandi (Forthcoming). Verbal -s variation in earlier African American English._Journal of English Linguistics._ 

**Coding Schema**
Sentence: The full sentence the target verb is in
Interviewee: Name of interviewee
      Example Levels: Jesse Rice, Henry Garry, etc.
State: Home state of interviewee 
      Levels: AL, GA, MS, NC, SC, TN
Region: Home region of interviewee 
      Levels: Gulf, Lower Coast, Mountain, Upper Coast
Verb: The specific verb used 
      Example Levels: do, get, go, etc. 
Verbal_S: Whether the verb was marked with -s (s) or not (0)
      Levels: s, 0
Type_of_Subject: Whether the subject associated with the verb was a full noun phrase (NP), personal pronoun (Pronoun), or null (Null)
      Levels: NP, Pronoun, Null
Person: The person of the subject
      Levels: First, Second, Third
Number: The number of the subject
      Levels: Plural, Singular, NA
Adjacency: Whether the verb is adjacent (A) or non-adjacent (N) to its subject
      Levels: A, N
Preceding_Segment: Whether the segment preceding the environment for verbal -s was a consonant (C), vowel (V), sibilant (S), or idiosyncratic (irregular)
      Environment: "She go__ to the store."  Preceding segment here would be the "o" of "go"
      Levels: C, irregular, S, V
Following_Segment: Whether the segment following the environment for verbal -s was a consonant (C), vowel (V), sibilant (S), or pause (P)
      Environment: "She go__ to the store."  Following segment here would be the "t" of "to"
      Levels: C, P, S, V
ThirdSingular: Whether the subject was third singular (T) or not (N)
      Levels: T, N
Verbal_aspect: Whether the verb was dynamic (dynamic), stative (state), or an auxiliary (aux)
      Levels: dynamic, state, aux
Aspect: Whether the phrase including the verb was habitual (habitual), non-habitual (non-habitual), or a discourse marker (discourse)
      Levels: habitual, non-habitual, discourse
Verb_Type: Whether the verb was one of the special verbs "do," "have," or "go" (dhg) or not (rest)
      Levels: dhg, rest


**Analysis Notes**
Some levels were removed from the analysis either because there was not enough tokens to warrant their analysis (discourse in Aspect and Null in Type_of_Subject) or because there was a potential flaw in the thinking of the original coding (P in Following_Segment).
      
