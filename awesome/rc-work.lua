vicious = require("vicious")
blingbling = require("blingbling")
handTiler = require("hand-tiler")
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
  names  = { "Emacs", "Chromium", "Term", "Extra" },
  layout = { layouts[1], layouts[10], layouts[6], layouts[1]
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
   { "quit", awesome.quit },
   { "dialog", '/home/jmassonx/jm-config/awesome/shutdown_dialog.sh' },
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
    pango_xx_large="size=\"xx-large\""
    pango_x_large="size=\"x-large\""
    pango_large="size=\"large\""
    pango_small="size=\"small\""
    pango_x_small="size=\"x-small\""
    pango_xx_small="size=\"xx-small\""
    pango_bold="weight=\"bold\""

-- function to run script
function run_script()
    local filedescriptor = io.popen(home_dir .. "ls -1 ~/Maildir/Intel/INBOX/new/ | wc -l")
    local value = filedescriptor:read()
    filedescriptor:close()
    return {value}
end

-- Mails
maillabel= widget({ type = "textbox" })
maillabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Mail </span>'
mailimage= widget({ type = "imagebox" })
mailimage.image= image("/home/jmassonx/jm-config/awesome/icons/email.png")
-- mailwidget = widget({
--    type = 'textbox',
--    name = 'mailwidget'
-- })
-- vicious.register(mailwidget, run_script, '$1')

-- mdir
-- gmailwidget = widget({ type = "textbox" })
-- vicious.register(gmailwidget, vicious.widgets.mdir, "$1/$2 ", 5, { home_dir ..'/Maildir/Gmail/INBOX/' })
-- eseowidget = widget({ type = "textbox" })
-- vicious.register(eseowidget, vicious.widgets.mdir, "$1/$2 ", 5, { home_dir ..'/Maildir/Eseo/INBOX/' })
-- openwidewidget = widget({ type = "textbox" })
-- vicious.register(openwidewidget, vicious.widgets.mdir, "$1/$2", 5, { home_dir ..'/Maildir/OpenWide/INBOX/' })
intelwidget = widget({ type = "textbox" })
vicious.register(intelwidget, vicious.widgets.mdir, "$1 / $2", 5, { home_dir .. '/Maildir/Intel/INBOX/' })

-- mpd
-- mpdwidget = widget({ type = "textbox" })
-- vicious.register(mpdwidget, vicious.widgets.mpd, " ${Title}, ${Album} ", 5)

-- packages
-- packagesimage= widget({ type = "imagebox" })
-- packagesimage.image= image("/home/jmassonx/jm-config/awesome/icons/update-2.png")
-- packageslabel= widget({ type = "textbox" })
-- packageslabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Packages </span>'
-- packageswidget = widget({ type = "textbox" })
-- vicious.register(packageswidget, vicious.widgets.pkg, "$1 ", 5, "Ubuntu")

-- org
todolabel= widget({ type = "textbox" })
todolabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Todo </span>'
workinglabel= widget({ type = "textbox" })
workinglabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Working </span>'
unmergedlabel= widget({ type = "textbox" })
unmergedlabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Unmerged </span>'

todowidget = widget({ type = "textbox" })
vicious.register(todowidget, vicious.widgets.org, "$1", 300, { home_dir ..'/org/todo.org' })
workingwidget = widget({ type = "textbox" })
vicious.register(workingwidget, vicious.widgets.org, "$2", 300, { home_dir ..'/org/todo.org' })
unmergedwidget = widget({ type = "textbox" })
vicious.register(unmergedwidget, vicious.widgets.org, "$3", 300, { home_dir ..'/org/todo.org' })

-- weather
-- weatherlabel= widget({ type = "textbox" })
-- weatherlabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Weather </span>'
-- weatherwidget = widget({ type = "textbox" })
-- vicious.register(weatherwidget, vicious.widgets.weather, '${tempc}°C', 1200, "LFBF")
-- vicious.register(weatherwidget, vicious.widgets.weather, '${city}, ${wind}, ${windmph}, ${windkmh}, ${sky}, ${weather}, ${tempf}, ${tempc}, ${humid}, ${dewf}, ${dewc}, ${press} ', 1200, "LFBF")

-- Create a textclock widget
mytextclock = awful.widget.textclock({ align = "right" })

-- org calendar
-- dofile(home_dir .. "/jm-config/awesome/modules/orglendar.lua")
-- orglendar.files = { home_dir .. "/org/todo.org"}
-- orglendar.register(mytextclock)


--shutdown widget
-- shutdown=blingbling.system.shutdownmenu(beautiful.shutdown,
--                                         beautiful.accept,
--                                         beautiful.cancel)
-- shutdown.resize= false
-- awful.widget.layout.margins[shutdown]={top=4}

--reboot widget
-- reboot=blingbling.system.rebootmenu(beautiful.reboot,
--                                     beautiful.accept,
--                                     beautiful.cancel)
-- reboot.resize = false
-- awful.widget.layout.margins[reboot]={top=4}

-- Date
datewidget = widget({ type = "textbox" })
vicious.register(datewidget, vicious.widgets.date, '<span color="#ff8700" '..pango_small..'>%b %d, %R</span>', 60)

--Cpu widget 
cpulabel= widget({ type = "textbox" })
cpulabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>CPU </span>'
cpu=blingbling.classical_graph.new()
cpu:set_font_size(8)
cpu:set_height(20)
cpu:set_width(100)
cpu:set_show_text(true)
cpu:set_label("$percent %")
cpu:set_graph_color("#00ccff00")
--Use transparency on graph line color to reduce the width of line with low resolution screen
cpu:set_graph_line_color("#ff330088")
cpu:set_filled(true)
cpu:set_h_margin(2)
cpu:set_background_color("#00000044")
cpu:set_filled_color("#00000099")
cpu:set_rounded_size(0.6)
vicious.register(cpu, vicious.widgets.cpu, '$1',2)

-- Initialize widget
cpudegreewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(cpudegreewidget, vicious.widgets.thermal, "$1 °C", 30, { "coretemp.0", "core"})
 
--Cores Widgets
-- corelabel=widget({ type = "textbox" })
-- corelabel.text='<span color="#ff8700" '..pango_large..'>Cores:</span>'
-- mycore1 = blingbling.value_text_box.new()
-- mycore1:set_width(25)
-- mycore1:set_height(16)
-- mycore1:set_filled(true)
-- mycore1:set_filled_color("#00000099")
-- mycore1:set_rounded_size(0.6)
-- mycore1:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
-- mycore1:set_font_size(8)
-- mycore1:set_background_color("#00000044")
-- mycore1:set_label("$percent%")
-- vicious.register(mycore1, vicious.widgets.cpu, "$2")

-- mycore2 = blingbling.value_text_box.new()
-- mycore2:set_width(25)
-- mycore2:set_height(16)
-- mycore2:set_filled(true)
-- mycore2:set_filled_color("#00000099")
-- mycore2:set_rounded_size(0.6)
-- mycore2:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
-- mycore2:set_font_size(8)
-- mycore2:set_background_color("#00000044")
-- mycore2:set_label("$percent%")
-- vicious.register(mycore2, vicious.widgets.cpu, "$3")

-- Mem Widget
memlabel= widget({ type = "textbox" })
memlabel.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>RAM </span>'
memwidget = blingbling.classical_graph.new()
memwidget:set_font_size(8)
memwidget:set_height(20)
memwidget:set_h_margin(2)
memwidget:set_width(100)
memwidget:set_filled(true)
memwidget:set_show_text(true)
memwidget:set_filled_color("#00000099")
memwidget:set_rounded_size(0.6)
--We just want the line of the graph
memwidget:set_graph_color("#00ccff00")
--Use transparency on graph line color to reduce the width of line with low resolution screen
memwidget:set_graph_line_color("#00ccff88")
memwidget:set_background_color("#00000044")
vicious.register(memwidget, vicious.widgets.mem, "$1", 2)
-- blingbling.popups.htop(memwidget.widget,
--                        { title_color = "#66CC00",
--                          user_color    = "#33CCFF",
--                          root_color= "#FF6600",
--                          terminal = "gnome-terminal"})

-- Initialize widget
memtextwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(memtextwidget, vicious.widgets.mem, "$2MB", 10)

-- Calendar widget
my_cal =blingbling.calendar.new({type = "imagebox", image = beautiful.calendar})
my_cal:set_cell_padding(4)
my_cal:set_title_font_size(11)
my_cal:set_font_size(10)
my_cal:set_inter_margin(3)
my_cal:set_columns_lines_titles_font_size(10)
my_cal:set_columns_lines_titles_text_color("#d4aa00ff")

-- Net Widget
my_net=blingbling.net.new()
my_net:set_height(20)
my_net:set_width(90)
my_net:set_v_margin(3)
my_net:set_graph_line_color("#00ccff00")
my_net:set_graph_color("#ff8700")
my_net:set_filled_color("#00000055")
my_net:set_show_text(false)

-- wifi widget
wifiwidget = widget({ type = "textbox" })
vicious.register(wifiwidget, vicious.widgets.wifi, ' <span color="#ff8700" '..pango_large..' '..pango_bold..'> ${ssid} </span>', 1, "wlan0")
wifispeedwidget = widget({ type = "textbox" })
vicious.register(wifispeedwidget, vicious.widgets.net, '${wlan0 up_kb}Kb   ${wlan0 down_kb}Kb', 1)
eth_up_speedwidget = widget({ type = "textbox" })
vicious.register(eth_up_speedwidget, vicious.widgets.net, '${eth0 up_kb}Kb', 1)
eth_down_speedwidget = widget({ type = "textbox" })
vicious.register(eth_down_speedwidget, vicious.widgets.net, '${eth0 down_kb}Kb', 1)

-- FS Widget
-- fshomelabel= widget({ type = "textbox", name = "fshomelabel" })
-- fshomelabel.text='<span color="#ff8700" '..pango_large..'>/home: </span>'
-- fshome = blingbling.value_text_box.new()
-- fshome:set_width(25)
-- fshome:set_height(16)
-- fshome:set_filled(true)
-- fshome:set_filled_color("#00000099")
-- fshome:set_rounded_size(0.6)
-- fshome:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
-- fshome:set_font_size(8)
-- fshome:set_background_color("#00000044")
-- fshome:set_label("$percent%")
-- vicious.register(fshome, vicious.widgets.fs, "${/home used_p}", 120 )

-- fsrootlabel= widget({ type = "textbox", name = "fsrootlabel" })
-- fsrootlabel.text='<span color="#ff8700" '..pango_large..' >Root </span>'
-- fsroot = blingbling.value_text_box.new()
-- fsroot:set_width(25)
-- fsroot:set_height(20)
-- fsroot:set_filled(true)
-- fsroot:set_filled_color("#00000099")
-- fsroot:set_rounded_size(0.6)
-- fsroot:set_values_text_color({{"#88aa00ff",0},{"#d4aa00ff", 0.5},{"#d45500ff",0.75}})
-- fsroot:set_font_size(12)
-- fsroot:set_background_color("#00000044")
-- fsroot:set_label("$percent%")
-- vicious.register(fsroot, vicious.widgets.fs, "${/ used_p}", 120 )

-- my_fs_data1=blingbling.progress_bar.new()
-- my_fs_data1:set_height(18)
-- my_fs_data1:set_width(40)
-- my_fs_data1:set_graph_color("#ff8700")
-- my_fs_data1:set_show_text(true)
-- my_fs_data1:set_horizontal(true)
-- vicious.register(my_fs_data1, vicious.widgets.fs, "${/ used_p}", 120 )

-- Volume
volume_label = widget({ type = "textbox"})
volume_label.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Vol </span>'
-- volumewidget = widget({ type = "textbox" })
-- vicious.register(volumewidget, vicious.widgets.volume, '$1', 1, "Master")
my_volume=blingbling.volume.new()
my_volume:set_height(20)
my_volume:set_v_margin(3)
my_volume:set_width(25)
my_volume:update_master()
my_volume:set_master_control()
my_volume:set_bar(true)
my_volume:set_background_graph_color("#00000099")
my_volume:set_graph_color("#ff8700")

-- Battery
battery_label = widget({ type = "textbox"})
battery_label.text='<span color="#ff8700" '..pango_large..' '..pango_bold..'>Bat </span>'
batwidget=blingbling.progress_graph.new()
batwidget:set_height(18)
batwidget:set_width(30)
batwidget:set_show_text(true)
batwidget:set_horizontal(true)
batwidget:set_filled(true)
-- batwidget = awful.widget.progressbar()
-- batwidget:set_width(8)
-- batwidget:set_height(14)
-- batwidget:set_vertical(true)
-- batwidget:set_background_color("#000000")
-- batwidget:set_border_color(nil)
-- batwidget:set_color("#00bfff")
vicious.register(batwidget, vicious.widgets.bat, "$2", 30, "BAT0")

-- use widget({ type = "textbox" }) for awesome < 3.5
separator = widget({ type = "textbox" })
-- use separator.text  = " :: " for awesome < 3.5
separator.text  = "    ::    "

space = widget({ type = "textbox" })
-- use separator.text  = " :: " for awesome < 3.5
space.text  = "  "

-- Calendar widget to attach to the textclock
-- dofile(home_dir .. "/jm-config/awesome/modules/calendar2.lua")
-- require('calendar2')
-- calendar2.addCalendarToWidget(mytextclock)

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
		space,
		mytextclock,
		space,
		my_cal.widget,
        	s == 1 and mysystray or nil,
	separator,
		my_volume.widget,
		space,
		volume_label,
	separator,
		batwidget.widget,
		space,
		battery_label,
	separator,
		cpudegreewidget,
		space,
		cpu.widget,
		space,
        	cpulabel,
	separator,
		memtextwidget,
		space,
		memwidget.widget,
		space,
		memlabel,
	separator,
		unmergedwidget,
		space,
		unmergedlabel,
	separator,
		workingwidget,
		space,
		workinglabel,
	separator,
		todowidget,
		space,
		todolabel,
	separator,
		intelwidget,
		space,
		mailimage,
	separator,
		eth_down_speedwidget,
		space,
		my_net.widget,
		space,
		eth_up_speedwidget,
	-- separator,
	-- 	wifispeedwidget,
	-- 	space,
	-- 	wifiwidget,
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
    -- awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    -- awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "Right",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "Left",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "Right", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "Left", function () awful.client.swap.byidx( -1)    end),
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

    -- awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    -- awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    -- awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    -- awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    -- awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    -- awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    awful.key({ modkey },            "d",     function ()
    		awful.util.spawn("dmenu_run -i -p 'Run command:' -nb '" .. 
    					    beautiful.bg_normal .. "' -nf '" .. beautiful.fg_normal .. 
    					    "' -sb '" .. beautiful.bg_focus .. 
    					    "' -sf '" .. beautiful.fg_focus .. "'") 
    end),

    awful.key({ modkey }, "o", function () awful.util.spawn("amixer set Master 4%+") end),
    awful.key({ modkey }, "i", function () awful.util.spawn("amixer set Master 4%-") end),
    awful.key({ modkey }, "p", function () awful.util.spawn("amixer -D pulse set Master toggle") end),


    awful.key({ modkey, "Shift" }, "s", function () awful.util.spawn("unity-control-center") end),
    awful.key({ modkey, "Shift" }, "p", function () awful.util.spawn("evince") end),
    awful.key({ modkey, "Shift" }, "e", function () awful.util.spawn("emacs") end),
    awful.key({ modkey, "Shift" }, "w", function () awful.util.spawn("chromium-browser") end),

    awful.key({ modkey }, "h", function ()
    		mywibox[mouse.screen].visible = not mywibox[mouse.screen].visible
		end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "s", function ()
        awful.prompt.run({ prompt = "Web search: " }, mypromptbox[mouse.screen].widget,
            function (command)
                awful.util.spawn("firefox 'http://yubnub.org/parser/parse?command="..command.."'", false)
                -- Switch to the web tag, where Firefox is, in this case tag 2
                if tags[mouse.screen][2] then awful.tag.viewonly(tags[mouse.screen][2]) end
            end)
    end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

clientkeys = awful.util.table.join(

    -- Alt key for tiling
    awful.key({ "Mod1",           },  "Left",     function (c) handTiler.tileTo(c, 'left')         end),
    awful.key({ "Mod1",           },  "Right",     function (c) handTiler.tileTo(c, 'right')        end),
    awful.key({ "Mod1",    "Shift" },  "Left",     function (c) handTiler.tileTo(c, 'left-top')     end),
    awful.key({ "Mod1",    "Control" },  "Left",     function (c) handTiler.tileTo(c, 'left-bottom')  end),
    awful.key({ "Mod1",    "Shift"},  "Right",     function (c) handTiler.tileTo(c, 'right-top')    end),
    awful.key({ "Mod1",    "Control"},  "Right",     function (c) handTiler.tileTo(c, 'right-bottom') end),
    awful.key({ "Mod1",           },  "Up",    function (c) handTiler.tileTo(c, 'top')    end),
    awful.key({ "Mod1",           },  "Down",  function (c) handTiler.tileTo(c, 'bottom') end),

    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    -- awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    -- awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    -- awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    -- awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    -- awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),

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
            c.maximized_horizontal = false
            c.maximized_vertical   = false
	    awful.mouse.client.move(c)
	    end),

    awful.key({ modkey, "Control"  }, "t", function () awful.client.moveresize(  0,  0,  1920 + 1920 - 80 -415,  1200 - 40 - 480) end),
    awful.key({ modkey, "Control"  }, "Down",  function () awful.client.moveresize(  0,  0,   0,  10) end),
    awful.key({ modkey, "Control"  }, "Up",    function () awful.client.moveresize(  0,  0,   0, -10) end),
    awful.key({ modkey, "Control"  }, "Left",  function () awful.client.moveresize(  0,  0, -10,  0) end),
    awful.key({ modkey, "Control"  }, "Right", function () awful.client.moveresize(  0,  0,  10,  0) end)

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
                     buttons = clientbuttons,
		     size_hints_honor = false} },
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
awful.util.spawn_with_shell("setxkbmap gb")
awful.util.spawn_with_shell("setxkbmap -option ctrl:swapcaps")
awful.util.spawn_with_shell("setxkbmap -option ctrl:nocaps")
