isLeapYear := method(year,
    if(year % 400 == 0, 
        true,
        if(year % 100 == 0, 
            false,
            year % 4 == 0
        )
    )
)

// Example usage:
year := 1996
if(isLeapYear(year), 
    "It's a leap year!", 
    "It's not a leap year."
) println