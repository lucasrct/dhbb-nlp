#!/bin/bash
for f in {1..35};
do ./apposDhbb ~/appos/old/${f}.conllu ~/appos/new/${f}.conllu head deprel misc > ~/appos/${f}.diff;
done
