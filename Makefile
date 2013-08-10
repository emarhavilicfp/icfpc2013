# the following are SML-NJ specific defines
SML = sml

solve: FORCE
	#echo 'use "compile-solve.sml";' | ${SML}
	ml-build solve.cm Top.main bin/solve.heap

check: FORCE
	ml-build solve.cm Top.check bin/check.heap
	./bin/check

solve-mlton: FORCE
	mllex lang/bv.lex
	mlyacc lang/bv.grm
	mlton -codegen c -profile time -profile-branch true -output bin/solve-mlton solve.mlb

reallyclean: clean
	${RM} parse/*.lex.* parse/*.grm.*
	find . -type f -name '*~' | xargs rm -rf

clean:
	find . -type d -name .cm | xargs rm -rf
	find . -type f | grep '~$$' | xargs ${RM}
	${RM} bin/solve.heap.*
	${RM} bin/solve-mlton

TAGS: clean
	${RM} TAGS
	bin/create-tags *.cm *.sml */*.lex */*.grm */*.sml

FORCE: 
