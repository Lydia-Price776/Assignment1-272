--Lydia Price ID: 20004521 Assignment One

--Top Level Function
adjust_date :: (Int,Int,Int) -> Int -> (Int, Int, Int)
adjust_date date offset
           | check_year date == False      = error "Year is out of the range 1600 to 3000"
           | check_month date == False     = error "Month is out of the range 1 to 12"
           | check_offset offset == False  = error "Offset is out of the range -25 to 25"
           | check_day date == False       = error "Date is not within the given month"
           | otherwise                     = shift_date date offset


-- Determines how to shift the date and returns the new date 
shift_date :: (Int,Int,Int) -> Int -> (Int,Int,Int)
shift_date date offset 
          | month == 12 && new_day > 31        = (new_day - 31, 1, year + 1) 
          | month == 1 && new_day <= 0         = (31 + new_day, 12, year - 1)
          | new_day <= 0                       = month_backward new_day month year
          | new_day > month_length month year  = month_forward new_day month year
          | otherwise                          = (new_day, month, year)
          where current_day = get_day date
                month = get_month date
                year = get_year date
                new_day = current_day + offset


-- Returns the new date in the previous Month                       
month_backward :: Int-> Int -> Int -> (Int, Int, Int) 
month_backward new_day month year = (day, month - 1, year) 
             where day = month_length (month - 1) year + new_day


--Returns the new date in the following Month
month_forward :: Int -> Int-> Int -> (Int, Int, Int) 
month_forward new_day month year = (day, month + 1, year) 
            where day = new_day - month_length month year 


-- Checks the Day is within the given Month 
check_day ::(Int, Int, Int) -> Bool
check_day date
        | day < 1                        = False
        | day > month_length month year  = False
        | otherwise                      = True 
        where day = get_day date 
              month = get_month date
              year = get_year date


-- Checks if the Year is valid 
check_year :: (Int, Int, Int) -> Bool
check_year x
          | get_year x < 1600  = False
          | get_year x > 3000  = False
          | otherwise          = True 


--Checks if the Month is valid 
check_month :: (Int, Int, Int) -> Bool
check_month x   
          | get_month x < 1   = False
          | get_month x > 12  = False
          | otherwise         = True 


--Checks if the offset is valid 
check_offset :: Int -> Bool
check_offset x   
          | x < -25    = False
          | x > 25     = False
          | otherwise  = True 


--Returns the number of days in a month 
month_length :: Int -> Int -> Int
month_length month year
           | member_of_list month [1,3,5,7,8,10,12]  = 31
           | member_of_list month [4,6,9,11]         = 30
           | month == 2 && is_leapyear year          = 29
           | otherwise                               = 28


--Checks if a given integer is a member of a given list
member_of_list :: Int -> [Int] -> Bool
member_of_list _ [] = False
member_of_list x (h:t)
              | x == h    = True
              | otherwise = member_of_list x t



-- Checks whether a given year is a leap year 
is_leapyear :: Int -> Bool 
is_leapyear year 
          | mod year 400 == 0  = True
          | mod year 100 == 0  = False 
          | mod year 4 == 0    = True
          | otherwise          = False 


-- Returns the Day component of a tuple
get_day :: (Int,Int, Int) -> Int
get_day (x, _, _) = x


-- Returns the Month component of a tuple
get_month :: (Int,Int, Int) -> Int
get_month (_, x, _) = x


-- Returns the Year component of a tuple
get_year :: (Int,Int, Int) -> Int
get_year (_, _, x) = x


