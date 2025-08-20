SELECT melid, performer, solo_info.title, solopart, chorus_count, instrument, style, tonalitytype, genre, rhythmfeel,
avgtempo, tempoclass, key, signature, recordingdate, composer, form, template, label, releasedate
FROM solo_info, track_info, composition_info, record_info 
WHERE solo_info.trackid = track_info.trackid 
AND solo_info.compid = composition_info.compid
AND solo_info.recordid = record_info.recordid
ORDER BY performer;