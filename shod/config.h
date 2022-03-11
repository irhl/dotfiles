struct Config config = {
	/*
	 * except for the foreground, colors fields are strings
	 * containing up to three elements delimited by colon:
	 * the body color, the color of the light 3D shadow,
	 * and the color of the dark 3D shadow.
	 */

	/* general configuration */
	.snap           = 8,            /* proximity of container edges to perform snap attraction */
	.font           = "fixed",      /* font for titles in titlebars */
	.ndesktops      = 10,           /* number of desktops per monitor */

	/* dock configuration */
	.dockwidth      = 64,           /* width of the dock (or its height, if it is horizontal) */
	.dockspace      = 64,           /* size of each dockapp (64 for windowmaker dockapps) */
	.dockgravity    = "E",          /* placement of the dock */
	.dockcolors     = "#121212:#2E3436:#000000",

	/* notification configuration */
	.notifgap       = 3,            /* gap, in pixels, between notifications */
	.notifgravity   = "NE",         /* placement of notifications */
	.notifcolors    = "#3465A4:#729FCF:#204A87",

	/* prompt configuration */
	.promptcolors   = "#3465A4:#729FCF:#204A87",

	/* container colors */
	.titlewidth = 24,
	.titlecolors = {
		[FOCUSED]   = "#000000:#000000:#000000",
		[UNFOCUSED] = "#000000:#000000:#000000",
		[URGENT]    = "#000000:#000000:#000000",
	},
	.foreground = {
		[FOCUSED]   = "#FFFFFF",
		[UNFOCUSED] = "#FFFFFF",
		[URGENT]    = "#FFFFFF",
	},

	/* border */
	.borderwidth = 5,
	.bordercolors = {
		[FOCUSED]   = "#000000:#000000:#000000",
		[UNFOCUSED] = "#000000:#000000:#000000",
		[URGENT]    = "#000000:#000000:#000000",
	},
};
