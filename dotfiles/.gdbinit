set prompt \033[31mgdb$ \033[0m
set print pretty on

define skip
    tbreak +1
    jump +1
end

define hook-quit
    set confirm off
end
