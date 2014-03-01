# html
#pandoc -S -o _output/snap-for-beginners.html title.txt \
#        01-install/01-install.md

# PDF
#pandoc --toc --variable version=0.0.1 -N --highlight-style=tango --latex-engine=xelatex --variable mainfont=Helvetica --variable monofont="Meslo LG S" --chapters title.txt 01-install/01-install.md 02-scaffolding/02-scaffolding.md 05-digestive-functors/05-digestive-functors.md -o _output/snap_for_beginners.pdf
pandoc --toc --variable version=0.0.1 -N --highlight-style=tango --latex-engine=xelatex --variable mainfont=Helvetica --variable monofont="Meslo LG L DZ" --chapters $(ls -d -1 `pwd`/_input/*.*) -o _output/snap_for_beginners.pdf
open `pwd`/_output/snap_for_beginners.pdf

# pandoc -S --epub-metadata.xml -o dist/haskell-snap.epub title.txt
