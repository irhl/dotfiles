require ("macro");

hilbish.opts.autocd = true
hilbish.opts.motd = false
hilbish.opts.greeting = false

print(hilbish.ver)

function prompt(fail)
    hilbish.prompt('-> ')
end

prompt()
