local ranch = require('ranch')

for _, path in ipairs(ranch.n1) do
    hilbish.prependPath(path)
end

for _, path in ipairs(ranch.n2) do
    hilbish.appendPath(path)
end

for key, value in pairs(ranch.n3) do
    os.setenv(key, value)
end

for alias, cmd in pairs(ranch.n4) do
    hilbish.alias(alias, cmd)
end

for opt, val in pairs(ranch.n5) do
    hilbish.opts[opt] = val
end
