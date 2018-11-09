;;start by defining the language to use. for this case its the racket language for scheme implementation
;;and defining all the dependencies we shall neeed for the abstractions to work
#lang racket
(require racket/system)
(require data-science-master)
(require plot)
(require math)
(require json)
;(require pict-master)

;;defclare export of these abstractions that are bindings to the provide from the file
(provide concat-tweets analyse-sent final-analysis)

;;define helper function that remove urls, punctuations and spaces from each tweet.
(define (preprocess-text text)
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase text)))))

;;this function is for reading the tweet database in memory
;;pull-tweets is a request made for tweets through the twitter API using twurl as provided for by twitter

;;we read the tweets after the request with th key "values" and put them into a byte using the inbuil
;;function with-output-to-string such that each request has a different byte associated to it
;;
(define (check-tweets pull-tweets)
  (let
      ((x (with-output-to-string
            (lambda () (system pull-tweets)))))
    (hash-ref (with-input-from-string x (lamda () (read-json))) 'values)))
    
  
;;the function below extracts neeeded data from the tweet i.e. text from each tweet
;; removing retweets as well
(define (t pull-tweets)
  (define (tweetdata (check-tweets pull-tweets))
  (let ([tmp (map (λ (x) (list (hash-ref x 'text))) tweetdata)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp))
    )
  )

;;the function above creates a list of strings
;;we have to extract each string and concatenate each with others into one long string
;;string-join appends list of strings into one long string 
(define (concat-tweets pull-tweets)
    (local[
           (define (concat-list list-tweets twt)
             (cond [(empty? list-tweets) twt]
                   [else (concat-list
                          (rest list-tweets)
                          (string-join (list
                                        twt
                                        (preprocess-text
                                         (first(first list-tweets))))))]
                   )
             )
           ](concat-list (t pull-tweets) "  ")) )

;;for sentiment analysis, extract each unique word the nu,ber of times it occurs in a tweet
;;using nrc lexicon, we can label each word with an emotional label
(define (analyse-sent text-from-tweet)
  (let* ((words (document->tokens text-from-tweet #:sort? #t))
         (sentiment (list->sentiment words #:lexicon 'nrc)))
    ;;aggregation across tokens
    (take sentiment 5)
    (let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
      (parameterize ((plot-width 800))
        (plot (list      ;;plotting the result on a bar i.e. histogram
               (tick-grid)
               (discrete-histogram
                (sort counts (λ (x y) (> (second x) (second y))))
                #:color "skyblue"
                #:line-color "gray"))
              #:x-label "Moods"
              #:y-label "Frequency")))
 ))

;;the abstraction below final-analysis is the one that calls all the previous abstractions
;;through a pull-tweets request then the result is analysed and sentiments plotted on a graph i.e. histogram
(define (final-analysis pull-tweets)
  (let
      (tresults (concat-tweets pull-tweets))
    (analyse-sent tresults)))
;;example of a pull-tweets request that uses twerl
;"twurl /1.1/tweets/search/30day/dev.json?query=Museveni search api&place_country=UG&fromDate=201807090000&toDate=201808090000"


;;demonstrations of how we can use the abstraction final-analysis
;;(final-analysis "/DUDEFROMLAROO/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=UgandaPolice search api&place_country=UG&fromDate=201809090000&toDate=201810090000")
;;(final-analysis "/DUDEFROMLAROO/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=VisitUganda search api&place_country=KE&fromDate=201710090000&toDate=201711090000")

;;Thank you


