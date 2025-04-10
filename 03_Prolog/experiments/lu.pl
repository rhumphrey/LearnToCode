% London Underground (partial knowledge base) 
% Facts - connected 
connected(bond_street,oxford_circus,central).
connected(oxford_circus,tottenham_court_road,central).
connected(bond_street,green_park,jubilee).
connected(green_park,charing_cross,jubilee).
connected(green_park,piccadilly_circus,piccadilly).
connected(piccadilly_circus,leicester_square,piccadilly).
connected(green_park,oxford_circus,victoria).
connected(oxford_circus,piccadilly_circus,bakerloo).
connected(piccadilly_circus,charing_cross,bakerloo).
connected(tottenham_court_road,leicester_square,northern).
connected(leicester_square,charing_cross,northern).

% Facts - nearby
nearby(bond_street,oxford_circus).
nearby(oxford_circus,tottenham_court_road).
nearby(bond_street,tottenham_court_road).
nearby(bond_street,green_park).
nearby(green_park,charing_cross).
nearby(bond_street,charing_cross).
nearby(green_park,piccadilly_circus).
nearby(piccadilly_circus,leicester_square).
nearby(green_park,leicester_square).
nearby(green_park,oxford_circus).
nearby(oxford_circus,piccadilly_circus).
nearby(piccadilly_circus,charing_cross).
nearby(oxford_circus,charing_cross).
nearby(tottenham_court_road,leicester_square).
nearby(leicester_square,charing_cross).
nearby(tottenham_court_road,charing_cross).

% Rules - nearby
nearby(X,Y):-connected(X,Y,L).
nearby(X,Y):-connected(X,Z,L),connected(Z,Y,L).

% Query
% ?-nearby(tottenham_court_road,W).
% ?-connected(tottenham_court_road,W,L).

% Facts - reachable
reachable(bond_street,charing_cross).
reachable(bond_street,green_park).
reachable(bond_street,leicester_square).
reachable(bond_street,oxford_circus).
reachable(bond_street,piccadilly_circus).
reachable(bond_street,tottenham_court_road).
reachable(green_park,charing_cross).
reachable(green_park,leicester_square).
reachable(green_park,oxford_circus).
reachable(green_park,piccadilly_circus).
reachable(green_park,tottenham_court_road).
reachable(leicester_square,charing_cross).
reachable(oxford_circus,charing_cross).
reachable(oxford_circus,leicester_square).
reachable(oxford_circus,piccadilly_circus).
reachable(oxford_circus,tottenham_court_road).
reachable(piccadilly_circus,charing_cross).
reachable(piccadilly_circus,leicester_square).
reachable(tottenham_court_road,charing_cross).
reachable(tottenham_court_road,leicester_square).

% Rules - reachable
reachable(X,Y,[]):-connected(X,Y,L).
reachable(X,Y,[Z|R]):-connected(X,Z,L),reachable(Z,Y,R).

% Query
% reachable(X,charing_cross,[A,B,C,D]).