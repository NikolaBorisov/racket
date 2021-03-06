#lang racket/base

(provide get-general-acks
         get-translating-acks
         get-authors)

(define (get-authors)
  (get-general-acks))

(define (get-general-acks)
  (string-append
   "The following individuals contributed to the implementation"
   " and documentation of Racket: "
   "Yavuz Arkun, "
   "Ian Barland, "
   "Eli Barzilay, "
   "Gann Bierner, "
   "Filipe Cabecinhas, "
   "Richard Cleis, "
   "John Clements, "
   "Richard Cobbe, "
   "Greg Cooper, "
   "Ryan Culpepper, "
   "Carl Eastlund, "
   "Moy Easwaran, "
   "Will Farr, "
   "Matthias Felleisen, "
   "Robby Findler, "
   "Kathi Fisler, "
   "Cormac Flanagan, "
   "Matthew Flatt, "
   "Sebastian Good, "
   "Paul Graunke, "
   "Kathy Gray, "
   "Dan Grossman, "
   "Dave Gurnell, "
   "Bruce Hauman, "
   "Dave Herman, "
   "Geoffrey S. Knauth, "
   "Mark Krentel, "
   "Shriram Krishnamurthi, "
   "Mario Latendresse, "
   "Guillaume Marceau, "
   "Jacob Matthews, "
   "Jay McCarthy, "
   "Mike T. McHenry, "
   "Philippe Meunier, "
   "Scott Owens, "
   "Jon Rafkind, "
   "Jamie Raymond, "
   "Grant Rettke, "
   "Paul Schlie, "
   "Dorai Sitaram, "
   "Mike Sperber, "
   "Paul Steckler, "
   "Jens Axel Søgaard, "
   "Francisco Solsona, "
   "Stevie Strickland, "
   "Sam Tobin-Hochstadt, "
   "Neil Van Dyke, "
   "David Van Horn, "
   "Anton van Straaten, "
   "Kevin Tew, "
   "Dale Vaillancourt, "
   "Dimitris Vyzovitis, "
   "Stephanie Weirich, "
   "Noel Welsh, "
   "Adam Wick, "
   "Danny Yoo, "
   "and "
   "ChongKai Zhu."))

(define (get-translating-acks)
  (string-append
   "Thanks to "
   "Ian Barland, "
   "Biep Durieux, "
   "Tim Hanson, "
   "Chihiro Kuraya, "
   "Philippe Meunier, "
   "Irina Mintiy, "
   "Sergey Semerikov, "
   "Jens Axel Søgaard, "
   "Francisco Solsona, "
   "Mike Sperber, "
   "Reini Urban, "
   "ChongKai Zhu, "
   "and "
   "Paolo Zoppetti "
   "for their help translating DrRacket's GUI to other languages."))
