

-spec initial_state() -> eqc_statem:symbolic_state().
-spec next_state(S :: eqc_statem:symbolic_state(), R :: eqc_statem:var(), C :: eqc_statem:call()) -> eqc_statem:symbolic_state().
-spec precondition(S :: eqc_statem:symbolic_state(), C :: eqc_statem:call()) -> boolean().
-spec postcondition(S :: eqc_statem:dynamic_state(), C :: eqc_statem:call(), R :: term()) -> boolean().
-spec command(S :: eqc_statem:symbolic_state()) -> eqc_gen:gen(eqc_statem:call()).



-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
