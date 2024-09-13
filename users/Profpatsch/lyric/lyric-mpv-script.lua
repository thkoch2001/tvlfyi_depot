-- get_lrc_subtitles.lua

-- Asynchronous callback function to handle the result of the 'get_subtitles' command
function handle_subtitle_result(success, result)
    if not success or result.status ~= 0 then
        mp.msg.error("Failed to get subtitles")
        mp.msg.error("Exit code: " .. tostring(result.exit_code))
        if result.stderr then
            mp.msg.error("Error: " .. result.stderr)
        end
        return
    end

    -- Extract the output from the external command
    local subtitle_file = result.stdout:match("^%s*(.-)%s*$")  -- trim any extra whitespace

    -- If no subtitle file was returned, stop
    if not subtitle_file or subtitle_file == "" then
        mp.msg.warn("No subtitle file found")
        return
    end

    -- Load the subtitle file in MPV
    mp.commandv("sub-add", subtitle_file)
    mp.msg.info("Loaded subtitle file: " .. subtitle_file)
end

-- Function to load the LRC subtitle file
function load_lrc_subtitles()
    -- Retrieve metadata for the currently playing file
    local artist = mp.get_property("metadata/by-key/artist")
    local album = mp.get_property("metadata/by-key/album")
    local title = mp.get_property("metadata/by-key/title")

    -- If any metadata is missing, print a warning and stop
    if not artist or not album or not title then
        mp.msg.warn("Artist, album, or title metadata is missing")
        return
    end

    -- Concatenate the metadata
    local query = string.format("%s %s %s", artist, album, title)

    -- Create the command array
    local cmd = {"@get_subtitles_command@", query}

    -- Run the external command asynchronously to get the subtitle file
    mp.command_native_async({
        name = "subprocess",
        args = cmd,
        capture_stdout = true,
        capture_stderr = true
    }, handle_subtitle_result)
end

-- Register the event that triggers when a file is loaded
mp.register_event("file-loaded", load_lrc_subtitles)
