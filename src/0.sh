cd ..
stack build
stack exec -- asteroidea -i ./src/flags.txt
# hp2ps -e8in -c asteroidea.hp
