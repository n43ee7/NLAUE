(* ::Package:: *)

(* ::Title:: *)
(*Evaluating Natural Language Arithmetic Expressions*)


(* To Load: << NLAUETWO`*)
(* To Run : NLAUETWO *)
BeginPackage["NLAUETWO`"]
NLAUE::usage="Returns a Graphical User Interface for evaluating natural language arithmetic expressions"
Begin["Private`"]


(* ::Subtitle::Closed:: *)
(*Main Function*)


ClearAll[NLAUE];


NLAUE:=DynamicModule[
	{expression=""},
	Panel[
		Column[
			{
			(* INPUT *)
			InputField[Dynamic[expression],String, ImageSize->Full, FieldHint->"Enter Expression Here", ImageSize->Large],
			(* UI SEPARATOR *)
			Panel["",Background-> Black, ImageSize->{Full,10}, FrameMargins->0],
			(* OUTPUT *)
			Panel[Style[Dynamic@composeResult[expression],50], ImageSize->{Full, Full}, Background->LightBlue,Alignment->Center]
			}
		],
		Style["Natural Language Arithmetic Expression Evaluator",Bold, 30],{{Top, Center}},
		ImageSize->{Full,Full},
		FrameMargins->Small,Background->Lighter[Gray, 0.9]
	]
]


(* ::Subtitle::Closed:: *)
(*Helper Functions*)


(* ::Section::Closed:: *)
(*createTokens (linear/lexical analysis)*)


ClearAll[createTokens];


createTokens[input_String] := Module[{tokenList = textToNumbers[StringSplit[injectSpace[input]]],
	beginningfluffPatterns =
		Alternatives[
			PatternSequence["what"|PatternSequence["how","much"], "is"|"does"],
			"what's",
			PatternSequence["tell","me","what"],
			""],
	endingfluffPatterns =
		Alternatives["is","equals"|"equal" | "?"],	
	ops},
	
	ops = {
		("+"|"plus") -> Plus,
		("-"|"minus") -> Subtract,
		{a___, "multiplied", "by", b___} :> {a, Times, b},
		("*"|"times") -> Times,
		{a___, "divided", "by", b___} :> {a, Divide, b},
		("/"|"\[Divide]") -> Divide,
		{a___, beginningfluffPatterns, b___} :> {a, $Fluff, b},
		{a___, endingfluffPatterns, b___} :>  {a, $Fluff, b}};
		
	tokenList //. ops
]

createTokens[___] := $Failed


(* ::Section::Closed:: *)
(*injectSpace (linear/lexical analysis)*)


ClearAll[injectSpace];


injectSpace[input_String]:=
	StringReplace[input, {
		numA:(DigitCharacter..)~~op:("+"|"-"|"*"|"/")~~numB:(DigitCharacter..):>numA<> " "<>op <>" "<> numB,
		char:("+"|"-"|"*"|"/")~~num:(DigitCharacter..):>char<> " "<>num,
		num:(DigitCharacter..)~~char:("+"|"-"|"*"|"/"):>num<> " "<>char,
		delim:("("|"["|")"|"]"|"?"):>" "<>delim<>" "
		}
	]


(* ::Section::Closed:: *)
(*textToNumbers (linear/lexical analysis)*)


ClearAll[$toNumbers, textToNumbers, notPowerOfTenQ];


$toNumbers = {
	"one"|"1" -> 1, "two"|"2" -> 2, "three"|"3" -> 3, "four"|"4" -> 4, "five"|"5" -> 5, "six"|"6" -> 6, "seven"|"7" -> 7, "eight"|"8" -> 8, "nine"|"9" -> 9,
	"eleven"|"11" -> 11, "twelve"|"12" -> 12, "thirteen"|"13" -> 13, "fourteen"|"14" -> 14, "fifteen"|"15" -> 15, "sixteen"|"16" -> 16, "seventeen"|"17" -> 17, "eighteen"|"18" -> 18, "nineteen"|"19" -> 19,
	"ten"|"10" -> 10, "twenty"|"20" -> 20, "thirty"|"30" -> 30, "forty"|"40" -> 40, "fifty"|"50" -> 50, "sixty"|"60" -> 60, "seventy"|"70" -> 70, "eighty"|"80" -> 80, "ninety"|"90" -> 90,
	"hundred"|"100" -> 100, "thousand"|"1000" -> 1000, "million"|"1000000" -> 1000000, "billion"|"1000000000" -> 1000000000, "trillion" -> 10^12, "quadrillion" -> 10^15, "quintillion" -> 10^18, "sextillion" -> 10^21,
	"tenth" -> 1/10, "hundredth" -> 1/100, "thousandth" -> 1/1000
};


textToNumbers[{args__}] := Module[
	{digits = (If[MatchQ[#, _String], ToLowerCase[#], #]&/@{args}) /. $toNumbers,
	lessThan100Pattern = 10 | PatternSequence[_?notPowerOfTenQ..],
	numConstructPattern},
	numConstructPattern = PatternSequence[lessThan100Pattern, _?(MatchQ[IntegerDigits[#], {1, 0..}]&)];
	
	digits = digits /. {a___, b_?NumericQ, "and", c_?NumericQ, d___} :> {a,b,c,d}; 
	digits = ReplaceRepeated[digits,
		{{a___, b_?notPowerOfTenQ, c_?notPowerOfTenQ, d___} :> {a, b+c, d} (*adding not powers of 10*),
				{aa___, bb_Integer, cc_?(MatchQ[IntegerDigits[#], {1, 0..}]&), dd___} :> {aa, bb*cc, dd}}] (* multiplying nums by magnitude*)
]

(*Module[
	{digits = (If[MatchQ[#,_String],ToLowerCase[#],#]&/@{args}) /. $toNumbers},
	(*the ordering of the following replacement rules matters so that "forty five thousand" doesn't get recognized as 5040*)
	digits = digits //. {{a___, b_Integer, c_?(IntegerQ[#]&&!MatchQ[IntegerDigits[#], {1, 0..}]&), d___} :> {a, b+c, d},
		{a___, b_Integer, c_?(MatchQ[IntegerDigits[#], {1, 0..}]&), d___} :> {a, b*c, d}}; Print[digits];
	digits
]*)


notPowerOfTenQ[num_] := IntegerQ[num]&&!MatchQ[IntegerDigits[num], {1, 0..}]


(* ::Section::Closed:: *)
(*parseInput (syntax analysis/parsing)*)


ClearAll[parseInput];


parseInput[""]:=""

parseInput[input_List]:=Module[
	{tokenList = input, mathExpr = Alternatives[_?NumericQ, _List]},

	(* Transformation rules *)
	tokenList = tokenList //. {
		{start___, "("|"[", opa:mathExpr, head_Symbol, opb:mathExpr, ")"|"]", end___}:>{start, {head, opa, opb}, end},
		{start___, opa:mathExpr, head:(Times | Divide), opb:mathExpr, end___}:>{start, {head, opa, opb}, end},
		{start___, opa:mathExpr, head:(Plus | Subtract), opb:mathExpr, end___}:>{start, {head, opa, opb}, end},
		{start___, opa:mathExpr, head_Symbol, opb:mathExpr, end___}:>{start, {head, opa, opb}, end}
	};
	
	(* de-fluffing *)
	tokenList = tokenList /. (Alternatives[
			{$Fluff, content_List, $Fluff},
			{$Fluff, content_List},
			{content_List, $Fluff}
		]):>content
]


(* ::Section::Closed:: *)
(*composeResult (semantic analysis)*)


ClearAll[composeResult];


composeResult[""]:= ""

composeResult[input_String] := Module[{res, heldRes},
	res = createTokens[input];
	res = parseInput[res];
	
	(* de-nesting lists for display *)
	heldRes = res //. {head_, opa_, opb_} :> HoldForm[head[opa,opb]];
	
	(* recursively computing result *) 
	(* exercise: accomplish the below result using Construct *)
	res = res //. {head_, opa_?NumericQ, opb_?NumericQ} :> head[opa,opb];
	
	(* check semantics (does it make sense?) *)
	If[NumericQ[res],
		Row[{heldRes, "\[LongEqual]", res}],
		Row[{Style["["<>input<>"]",15, FontColor->Blue],Style[" is not supported ",15, FontColor->Gray]}]]
]


(* ::Subtitle:: *)
(*End Package*)


End[]
EndPackage[]
