-- This function formats the current timestamp in the [mm:ss.ms] format
function format_timestamp(seconds)
    local minutes = math.floor(seconds / 60)
    local seconds = seconds % 60
    return string.format("[%02d:%05.2f]", minutes, seconds)
end

-- Get the user’s cache directory
local cache_dir = os.getenv("XDG_CACHE_HOME") or os.getenv("HOME") .. "/.cache"

-- This function writes the timestamp to the LRC file
function write_timestamp_to_lrc()
    local filename = mp.get_property("path")
    if not filename then
        mp.msg.warn("No file currently playing.")
        return
    end

    -- Extract metadata for artist and title
    local artist = mp.get_property("metadata/by-key/ARTIST", "Unknown Artist")
    local title = mp.get_property("metadata/by-key/TITLE", "Unknown Title")

    -- Construct the lrc dir
    local dir = cache_dir .. "/lyric/timed"
    local lrc_filename = string.format("%s/%s - %s.lrc", dir, artist, title)

    -- Get current playback time
    local current_time = mp.get_property_number("time-pos", 0)
    local formatted_time = format_timestamp(current_time)

    -- Append the timestamp to the LRC file
    local file = io.open(lrc_filename, "a")
    if file then
        file:write(formatted_time .. "\n")
        file:close()
        mp.msg.info("Timestamp " .. formatted_time .. " added to " .. lrc_filename)
    else
        mp.msg.error("Failed to open " .. lrc_filename)
    end
end

-- Bind Ctrl+l to the function that writes the timestamp
mp.add_key_binding("Ctrl+l", "insert_timestamp", write_timestamp_to_lrc)
