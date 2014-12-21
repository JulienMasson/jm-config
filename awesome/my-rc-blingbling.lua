vicious = require("vicious")
blingbling = require("blingbling")
-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- Load Debian menu entries
require("debian.menu")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

local home_dir   = os.getenv("HOME")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(home_dir .. "/jm-config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which will hold all screen tags.
tags = {
  names  = { "Emacs", "Firefox", "Term", "Extra" },
  layout = { layouts[2], layouts[10], layouts[1], layouts[1]
}}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tags.names, s, tags.layout)
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox

--pango
    pango_small="size=\"small\""
    pango_x_small="size=\"x-small\""
    pango_xx_small="size=\"xx-small\""
    pango_bold="weight=\"bold\""

-- --shutdown widget
--     shutdown=blingbling.system.shutdownmenu(beautiful.shutdown, 
--                                             beautiful.accept, 
--                                             beautiful.cancel)
--     shutdown.resize= false
--     awful.widget.layout.margins[shutdown]={top=4}
-- --reboot widget
--     reboot=blingbling.system.rebootmenu(beautiful.reboot, 
--                                         beautiful.accept, 
--                                         beautiful.cancel)
--     reboot.resize = false
--     awful.widget.layout.margins[reboot]={top=4}
--     -- Date
--     datewidget = widget({ type = "textbox" })
--     vicious.register(datewidget, vicious.widgets.date, "<span color=\""..beautiful.text_font_color_1.."\" "..pango_small..">%b %d, %R</span>", 60)

--Cpu widget 
cpulabel= widget({ type = "textbox" })
cpulabel.text='<span color="#ff8700" '..pango_small..' '..pango_bold..'>CPU: </span>'
cpu=blingbling.classical_graph.new()
cpu:set_font_size(8)
cpu:set_height(16)
cpu:set_width(150)
cpu:set_show_text(true)
cpu:set_label("Load: $percent %")
cpu:set_graph_color("#00ccff00")
--Use transparency on graph line color to reduce the width of line with low resolution screen
cpu:set_graph_line_color("#ff330088")
cpu:set_filled(true)
cpu:set_h_margin(2)
cpu:set_background_color("#00000044")
cpu:set_filled_color("#00000099")
cpu:set_rounded_size(0.6)
vicious.register(cpu, vicious.widgets.cpu, '$1',2)
 
--Cores Widgets
corelabel=widget({ type = "textbox" })
corelabel.text='<span color="#ff8700" '..pango_small..'>Cores:</span>'
mycore1 = blingbling.value_text_box.new()
mycore1:set_width(25)
mycore1:set_height(16)
mycore1:set_filled(true)
mycore1:set_filled_color("#00000099")
mycore1:set_rounded_size(0.6)
mycore1:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
mycore1:set_font_size(8)
mycore1:set_background_color("#00000044")
mycore1:set_label("$percent%")
vicious.register(mycore1, vicious.widgets.cpu, "$2")

mycore2 = blingbling.value_text_box.new()
mycore2:set_width(25)
mycore2:set_height(16)
mycore2:set_filled(true)
mycore2:set_filled_color("#00000099")
mycore2:set_rounded_size(0.6)
mycore2:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
mycore2:set_font_size(8)
mycore2:set_background_color("#00000044")
mycore2:set_label("$percent%")
vicious.register(mycore2, vicious.widgets.cpu, "$3")

-- Mem Widget
memlabel= widget({ type = "textbox" })
memlabel.text='<span color="#ff8700" '..pango_small..'>MEM: </span>'
memwidget = blingbling.classical_graph.new()
memwidget:set_font_size(8)
memwidget:set_height(16)
memwidget:set_h_margin(2)
memwidget:set_width(150)
memwidget:set_filled(true)
memwidget:set_show_text(true)
memwidget:set_filled_color("#00000099")
memwidget:set_rounded_size(0.6)
--We just want the line of the graph
memwidget:set_graph_color("#00ccff00")
--Use transparency on graph line color to reduce the width of line with low resolution screen
memwidget:set_graph_line_color("#00ccff88")
memwidget:set_background_color("#00000044")
vicious.register(memwidget, vicious.widgets.mem, "$1", 5)

-- Calendar widget
my_cal =blingbling.calendar.new({type = "imagebox", image = beautiful.calendar})
my_cal:set_cell_padding(2)
my_cal:set_title_font_size(9)
my_cal:set_font_size(8)
my_cal:set_inter_margin(1)
my_cal:set_columns_lines_titles_font_size(8)
my_cal:set_columns_lines_titles_text_color("#d4aa00ff")

-- Net Widget
netwidget = widget({ type = "textbox", name = "netwidget" })
netwidget.text='<span '..pango_small..'><span color="#ff8700">NET:</span></span>'
my_net=blingbling.net.new()
my_net:set_height(18)
my_net:set_width(88)
my_net:set_v_margin(3)
my_net:set_graph_line_color("#00ccff00")
my_net:set_graph_color("#ff8700")
my_net:set_filled_color("#00000055")
my_net:set_show_text(true)


-- FS Widget
fshomelabel= widget({ type = "textbox", name = "fshomelabel" })
fshomelabel.text='<span color="#ff8700" '..pango_small..'>/home: </span>'
fshome = blingbling.value_text_box.new()
fshome:set_width(25)
fshome:set_height(16)
fshome:set_filled(true)
fshome:set_filled_color("#00000099")
fshome:set_rounded_size(0.6)
fshome:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
fshome:set_font_size(8)
fshome:set_background_color("#00000044")
fshome:set_label("$percent%")
vicious.register(fshome, vicious.widgets.fs, "${/home used_p}", 120 )

fsrootlabel= widget({ type = "textbox", name = "fsrootlabel" })
fsrootlabel.text='<span color="#ff8700" '..pango_small..'>root: </span>'
fsroot = blingbling.value_text_box.new()
fsroot:set_width(25)
fsroot:set_height(16)
fsroot:set_filled(true)
fsroot:set_filled_color("#00000099")
fsroot:set_rounded_size(0.6)
fsroot:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
fsroot:set_font_size(8)
fsroot:set_background_color("#00000044")
fsroot:set_label("$percent%")
vicious.register(fsroot, vicious.widgets.fs, "${/ used_p}", 120 )

--Volume
volume_label = widget({ type = "textbox"})
volume_label.text='<span '..pango_small..'><span color="#ff8700">Vol.: </span></span>'
my_volume=blingbling.volume.new()
my_volume:set_height(16)
my_volume:set_v_margin(3)
my_volume:set_width(20)
my_volume:update_master()
my_volume:set_master_control()
my_volume:set_bar(true)
my_volume:set_background_graph_color("#00000099")
my_volume:set_graph_color("#ff8700")

-- use widget({ type = "textbox" }) for awesome < 3.5
separator = widget({ type = "textbox" })
-- use separator.text  = " :: " for awesome < 3.5
separator.text  = " :: "

-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- Calendar widget to attach to the textclock
dofile(home_dir .. "/jm-config/awesome/modules/calendar2.lua")
require('calendar2')
calendar2.addCalendarToWidget(mytextclock)

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({ width=250 })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
	mytextclock,
	separator,
	my_net.widget,
	netwidget,
	separator,
	fshome.widget,
	fshomelabel,
	separator,
	fsroot.widget,
	fsrootlabel,
	separator,
	my_volume.widget,
	volume_label,
	separator,
	memwidget.widget,
	memlabel,
	separator,
	my_cal.widget,
	separator,
        cpu.widget,
        cpulabel,
        separator,
        mycore1.widget,
        mycore2.widget,
        corelabel,
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    awful.key({ modkey },            "d",     function ()
    		awful.util.spawn("dmenu_run -i -p 'Run command:' -nb '" .. 
    					    beautiful.bg_normal .. "' -nf '" .. beautiful.fg_normal .. 
    					    "' -sb '" .. beautiful.bg_focus .. 
    					    "' -sf '" .. beautiful.fg_focus .. "'") 
    end),


    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),

    -- add in your .xinitrc
    -- xscreensaver -nosplash &
    awful.key({ modkey, "Control" }, "l", function () awful.util.spawn("xscreensaver-command -lock") end),

    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
	    end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { class = "gimp" },
      properties = { floating = true } },
    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}


-- Autostart
awful.util.spawn_with_shell("numlockx on")
awful.util.spawn_with_shell("setxkbmap fr")