% Facts
rule(q0, 1, q0, 1, right).                          % Defines a rule for state q0 with input 1: stay in q0, write 1, move right.
rule(q0, b, qf, 1, stay).                           % Defines a rule for state q0 with input blank: change to qf, write 1, stay.

% Rules
turing(Tape0, Tape) :-
    perform(q0, [], Ls, Tape0, Rs),                 % Start the Turing machine with state q0 and an empty left stack.
    reverse(Ls, Ls1),                               % Reverse the left stack to get the proper order.
    append(Ls1, Rs, Tape).                          % Concatenate the left and right stacks to form the final tape.

perform(qf, Ls, Ls, Rs, Rs) :- !.                   % If the state is qf (final), stop and return the stacks as they are.
perform(Q0, Ls0, Ls, Rs0, Rs) :-
    symbol(Rs0, Sym, RsRest),                       % Get the current symbol and the rest of the right stack.
    once(rule(Q0, Sym, Q1, NewSym, Action)),        % Apply the rule based on the current state and symbol.
    action(Action, Ls0, Ls1, [NewSym|RsRest], Rs1), % Perform the action specified by the rule.
    perform(Q1, Ls1, Ls, Rs1, Rs).                  % Recursively call perform with the new state and stacks.

symbol([], b, []).                                  % If the right stack is empty, return a blank symbol.
symbol([Sym|Rs], Sym, Rs).                          % Otherwise, return the first symbol and the rest of the stack.

action(left, Ls0, Ls, Rs0, Rs) :- left(Ls0, Ls, Rs0, Rs). % Define action for moving left.
action(stay, Ls, Ls, Rs, Rs).                             % Define action for staying.
action(right, Ls0, [Sym|Ls0], [Sym|Rs], Rs).              % Define action for moving right.

left([], [], Rs0, [b|Rs0]).                         % If moving left with an empty left stack, write a blank and update the right stack.
left([L|Ls], Ls, Rs, [L|Rs]).                       % Otherwise, pop from the left stack and push onto the right stack.

% Query
% ?- turing([1,1,1], Ts). 
% Ts = [1, 1, 1, 1] ;     
