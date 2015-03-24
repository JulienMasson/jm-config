---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
--  * (c) org-awesome, Damien Leone
---------------------------------------------------

-- {{{ Grab environment
local io = { lines = io.lines }
local setmetatable = setmetatable
local string = { find = string.find }
local os = {
    time = os.time,
    date = os.date
}
-- }}}


-- Org: provides agenda statistics for Emacs org-mode
module("vicious.widgets.org")


-- {{{ OrgMode widget type
local function worker(format, warg)
    if not warg then return end

    -- Initialize counters
    local count = { todo = 0, working = 0, unmerged = 0 }

    -- Get data from agenda files
    for i=1, #warg do
       for line in io.lines(warg[i]) do
          local todo = string.find(line, "TODO")
          local working    = string.find(line, "WORKING")
          local unmerged  = string.find(line, "UNMERGED")

       	  if (todo) then
       	     count.todo   = count.todo   + 1
       	  end

	  if (working) then
       	     count.working   = count.working   + 1
       	  end

       	  if (unmerged) then
       	     count.unmerged   = count.unmerged   + 1
       	  end

       end
    end

    return {count.todo, count.working, count.unmerged}
end
-- }}}

setmetatable(_M, { __call = function(_, ...) return worker(...) end })
