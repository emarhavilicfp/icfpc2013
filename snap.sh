git archive --format tar --prefix=emarhavil-icfpc-2013/ -v HEAD | bzip2 > snapshot.tbz2
SNAP=$(sha256sum snapshot.tbz2 | sed -e 's/ .*//')
mv snapshot.tbz2 $SNAP.tbz2
git branch snap-$SNAP HEAD
git push origin snap-$SNAP
