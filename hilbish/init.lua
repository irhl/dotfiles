if os.getenv 'TERM' == 'linux' then
    os.setenv('XDG_RUNTIME_DIR', '/tmp/1000')
    os.execute('mkdir -p ' .. os.getenv('XDG_RUNTIME_DIR'))

    hilbish.run 'sway'
    hilbush.run 'amixer sset 'Master' 88% > /dev/null 2>&1'
end

local ranci = require('ranci')
print(hilbish.ver)

function prompt(fail)
    hilbish.prompt('-> ')
end

prompt()
