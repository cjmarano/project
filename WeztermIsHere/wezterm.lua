-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices.

config.font = wezterm.font 'JetBrainsMono Nerd Font Mono'

-- For example, changing the initial geometry for new windows:
config.initial_cols =110
config.initial_rows = 60

-- or, changing the font size and color scheme.
config.font_size = 18
config.color_scheme = 'MaterialDesignColors'

-- set default working directory.
config.default_cwd = "$HOME/project/"

config.window_close_confirmation = 'NeverPrompt'

-- Finally, return the configuration to wezterm:
return config


