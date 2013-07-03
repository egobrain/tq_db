exec erl +K true +A 2 \
    -pa \
        ebin \
        deps/*/ebin \
    -boot start_sasl \
    -sname console \
    -s tq_db 
