% Define authors
author(jrr_tolkien).
author(alan_garner).
author(judith_cooper).
author(george_rr_martin).

% Define books with their respective authors
book(the_hobbit, jrr_tolkien).
book(the_lord_of_the_rings, jrr_tolkien).
book(the_weirdstone_of_brisingamen, alan_garner).
book(the_moon_of_gomrath, alan_garner).
book(the_witchs_daughter, judith_cooper).
book(a_game_of_thrones, george_rr_martin).
book(a_clash_of_kings, george_rr_martin).

% Rule to find books by an author
books_by_author(Author, Book) :-
    book(Book, Author).

% Rule to find authors of a book
author_of_book(Book, Author) :-
    book(Book, Author).
