// Define constants
SPECIES_SIZE := 4
TESTS_SIZE := 5

// Simple sum function for debug purposes
arraySum := method(sim,
    sim sum
)

// Coefficient functions follow
// Simple matching
ssm := method(sim,
    (sim at(0) + sim at(3)) / sim sum
)

// Jaccard
sj := method(sim,
    if(sim at(0) + sim at(1) + sim at(2) == 0, 0.0, sim at(0) / (sim at(0) + sim at(1) + sim at(2)))
)

// Dice
sd := method(sim,
    if(sim at(0) + sim at(1) + sim at(2) == 0, 0.0, (2 * sim at(0)) / ((2 * sim at(0)) + sim at(1) + sim at(2)))
)

// Hamann
sh := method(sim,
    ((sim at(0) + sim at(3)) - (sim at(1) + sim at(2))) / sim sum
)

// Kulczynski
sk2 := method(sim,
    if((sim at(0) + sim at(1)) == 0 or (sim at(0) + sim at(2)) == 0, 0.0, ((sim at(0) / (sim at(0) + sim at(1))) + (sim at(0) / (sim at(0) + sim at(2)))) / 2.0)
)

// Ochiai
so := method(sim,
    if((sim at(0) + sim at(1)) == 0 or (sim at(0) + sim at(2)) == 0, 0.0, sim at(0) / ((sim at(0) + sim at(1)) * (sim at(0) + sim at(2))) sqrt)
)

// Pattern difference
dp := method(sim,
    (2 * ((sim at(1) * sim at(2)) sqrt)) / sim sum
)

// Rogers and Tanimoto
srt := method(sim,
    (sim at(0) + sim at(3)) / ((sim at(0) + sim at(3)) + (2 * (sim at(1) + sim at(2))))
)

// Total difference
dt := method(sim,
    (sim at(1) + sim at(2)) / sim sum
)

// Check that a charcter is only + or -
isSign := method(char, 
    if(char == 43, return true)
    if(char == 45, return true)
    return false
)

// Error checking function to make sure all the characters in a string isSign
isValid := method(str,
    isValidSign := true
    if(str size != 5, isValidSign = false, 
        str foreach(c, 
            if(isSign(c) == false, 
                isValidSign = false
                break
            )
        )
    )
    isValidSign
)

// Five characters maximum
fcm := method(number,
    number asString exSlice(0, 5)
)

// Define the table as a list of lists (similar to a 2D array)
table := list(
    list("-", "-", "+", "+", "+"),  // Klebsiella spp.
    list("-", "+", "+", "+", "-"),  // Enterobacter spp.
    list("+", "+", "-", "+", "-"),  // Citrobacter spp.
    list("-", "+", "-", "-", "-")   // Escherichia coli
)

// Define the species list
speciesList := list("Klebsiella spp.  ", "Enterobacter spp.", "Citrobacter spp.", "Escherichia coli")

// Define the tests list
testsList := list("H2S", "Motility", "Inositol", "Citrate", "Urea")

// Display the tests list
"Test list\n---------\n" print
testsList foreach(index, test, 
    (index + 1 .. ". " .. test) println
)

// Input validation loop
unknownOtu := ""
flag := false
while(flag not,
    "Enter the +ve and -ve results of the above tests as a string of length 5 containing only + or - characters (extra characters will be ignored): " interpolate(testsList size) print
    unknownOtu = File standardInput readLine
    if(isValid(unknownOtu), 
        flag = true,
        "Invalid input. Please try again.\n" print
    )
) 

// Print the valid input
("Your entered: " .. unknownOtu .. "\n") println

// Print id table and unknown
"\nIdentification table\n--------------------\n" print
"Species\t\t\t" print
testsList foreach(index, test, (index + 1 .. "\t") print)
"\n" print

table foreach(speciesIndex, speciesRow,
    (speciesList at(speciesIndex) .. "\t") print
    speciesRow foreach(testIndex, testResult, (testResult .. "\t") print)
    "\n" print
)

"\nUnknown OTU\t\t" print
unknownOtu foreach(i, v, (unknownOtu inSlice(i, i) .. "\t") print)
"\n\n" print

// Print the Coefficients legend
"Coefficients (abbrv.) \n--------------------\n" print
"Simple matching (Ssm), Jaccard (Sj), Dice (Sd), Hamann (Sh), Kulczynski (Shk2), Ochiai (So), Pattern difference (Dp), Rogers and Tanimoto (Srt), Total difference (Dt) \n" print
"Higher values for coefficients indicate closest similarity (except Dp and Dt where a higher value indicates greater difference\n\n" print

// Print the similarity table
"Similarity table for coefficients\n----------------\n" print
"Species\t\t\tSsm\tSj\tSd\tSh\tShk2\tSo\tDp\tSrt\tDt\n" print

for(s, 0, SPECIES_SIZE - 1,
    sim := list(0, 0, 0, 0)
    for(t, 0, TESTS_SIZE - 1,
        // table at(s) at (t) print
        // (" vs " .. unknownOtu inSlice(t, t))  println
        if(unknownOtu inSlice(t, t) == "+" and ((table at(s) at(t) == "+") or (table at(s) at(t) == "v")),
            sim atPut(0, sim at(0) + 1)
        )
        if(unknownOtu inSlice(t, t) == "+" and ((table at(s) at(t) == "-") or (table at(s) at(t) == "v")),
            sim atPut(1, sim at(1) + 1)
        )
        if(unknownOtu inSlice(t, t) == "-" and ((table at(s) at(t) == "+") or (table at(s) at(t) == "v")),
            sim atPut(2, sim at(2) + 1)
        )
        if(unknownOtu inSlice(t, t) == "-" and ((table at(s) at(t) == "-") or (table at(s) at(t) == "v")),
            sim atPut(3, sim at(3) + 1)
        )
    )
    // sim println
    // print coefficients
    (speciesList at(s) .. "\t") print
    (fcm(ssm(sim)) .. "\t") print
    (fcm(sj(sim)) .. "\t") print
    (fcm(sd(sim)) .. "\t") print
    (fcm(sh(sim)) .. "\t") print
    (fcm(sk2(sim)) .. "\t") print
    (fcm(so(sim)) .. "\t") print
    (fcm(dp(sim)) .. "\t") print
    (fcm(srt(sim)) .. "\t") print
    (fcm(dt(sim)) .. "\t") println
)