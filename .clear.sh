for f in *.zip; do zip -qd "$f" "__MACOSX/*"; done
for f in *.zip; do zip -qd "$f" "*/.DS_Store"; done
