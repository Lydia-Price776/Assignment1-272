# Assignment1-272
Assignment 1 of 159.272 Haskell

## Assignment Specifications 

Write a Haskell function that takes a date and an offset as parameters and returns the date that 
is a certain number of days before or after the input date. The number of days is given by the offset parameter.

The top-level function must have the following function declaration:

`adjust_date :: (Int, Int, Int) -> Int -> (Int, Int, Int)`

The first parameter is a tuple that specifies a day,month and year.
The date must be a valid date between (1, 1, 1600) and (31, 12, 3000). 
The second parameter specifies an offset between -25 and 25. If the offset is negative, 
then the resulting date is before the given date; if it is positive, the resulting date is after the given date. 

The top-level function may call other Haskell functions to achieve the task.

## Marking Specifications 

- A set of Haskell functions that do not depend on any modules to be imported.
- Each function needs to have a comment explaining its purpose.
- The top-level function must adhere to the function declaration shown above.
-  If an incorrect date or offset is given as parameter, i.e. a date or offset not within the
permissible range, a sensible error message needs to be displayed.
