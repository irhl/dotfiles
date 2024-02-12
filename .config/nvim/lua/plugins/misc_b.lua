-- echasnovski/mini.statusline
local statusline = require 'mini.statusline';

local function group()
	local mode, mode_hl = statusline.section_mode    ({ trunc_width = 999 });
	local filename      = statusline.section_filename({ trunc_width = 999 });
	local fileinfo      = statusline.section_fileinfo({ trunc_width = 999 });
        local location      = statusline.section_location({ trunc_width = 999 })

	mode = string.lower(mode);

	return statusline.combine_groups({
		{ hl = mode_hl,                  strings = { mode } },
		'%<', -- Mark general truncate point
		{ hl = 'MiniStatuslineFilename', strings = { filename } },
		'%=', -- End left alignment
		{ hl = 'MiniStatuslineFileinfo', strings = { location } },
		{ hl = 'MiniStatuslineDevinfo',  strings = { fileinfo } },
	})
end

-- echasnovski/mini.starter
require 'mini.starter'.setup {
	evaluate_single = true,
}

statusline.setup {
	use_icons = false,
	content = {
		inactive = group,
		active = group
	}
}
