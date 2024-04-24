% Facts: Define pet and color relationships
pet(yowlie, cat).
pet(socks, dog).
pet(pink, mouse).
color(yowlie, tortie).
color(socks, tricolor).
color(pink, pink).

% Rules: Define a rule to check if a pet is a certain color
is_color(Pet, Color) :- pet(Pet, _), color(Pet, Color).

% When used as an argument in a Prolog rule or fact, the underscore acts as an anonymous variable.
% Unlike named variables (which start with an uppercase letter), anonymous variables are not bound to any specific value.
% They are placeholders used when we want to ignore a particular argument or when we don’t need to refer to it explicitly.

% Queries / Goals
% ?- pet(yowlie, What).     % What type of pet is Yowlie?
% ?- color(socks, What).    % What color is Socks?
% ?- is_color(pink, pink).  % Is Pink pink in color?
% ?- color(_, tortie).      % Find any pet with a tortie color.

% In queries, the underscore can act as a wildcard. It matches any value without binding it to a variable.