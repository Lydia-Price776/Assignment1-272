--Lydia Price ID: 20004521 Assignment One

--adjust_date :: (Int,Int,Int) -> Int -> (Int, Int, Int)



-- Checks the input is valid 
-- Could possibly combine this with the adjust date and where it is true have a shift_date function?
check_valid :: (Int, Int, Int) -> Int -> Bool
check_valid x y
           | check_year x == False    = error "Year is out of the range 1600 to 3000"
           | check_month x == False   = error "Month is out of the range 1 to 12"
           | y > 25 || y < -25        = error "Offset is out of the range -25 to 25"
           | check_day x == False     = error "Date is not within the given month"
           | otherwise                = True
          

-- Checks the day is within the given month 
check_day ::(Int, Int, Int) -> Bool
check_day x
        | get_day x < 1               = False
        | get_day x > month_length x  = False
        | otherwise                   = True 


-- Checks if the Year is valid 
check_year :: (Int, Int, Int) -> Bool
check_year x
          | get_year x < 1600  = False
          | get_year x > 3000  = False
          | otherwise          = True 



--Checks if the month is valid 
check_month :: (Int, Int, Int) -> Bool
check_month x   
          | get_month x < 1   = False
          | get_month x > 12  = False
          | otherwise         = True 

           

--This function checks the number of days in a month 
month_length :: (Int, Int, Int) -> Int
month_length x
           | member_of_list (get_month x) [1,3,5,7,8,10,12]  = 31
           | member_of_list (get_month x) [4,6,9,11]         = 30
           | get_month x == 2 && is_leapyear (get_year x)    = 29
           | otherwise                                       = 28



--This Function checks if a given integer is a member of a given list
member_of_list :: Int -> [Int] -> Bool
member_of_list _ [] = False
member_of_list x (h:t)
              | x == h    = True
              | otherwise = member_of_list x t



-- This function checks whether a given year is a leap year and returns True if it is,
-- and false if it isn't 
is_leapyear :: Int -> Bool 
is_leapyear year 
          | mod year 400 == 0       = True
          | mod year 100 == 0       = False 
          | mod year 4 == 0         = True
          | otherwise               = False 



-- This function returns the Day component of the tuple
get_day :: (Int,Int, Int) -> Int
get_day (x, _, _) = x



-- This function returns the Month component of the tuple
get_month :: (Int,Int, Int) -> Int
get_month (_, x, _) = x



-- This function returns the Year component of the tuple
get_year :: (Int,Int, Int) -> Int
get_year (_, _, x) = x


