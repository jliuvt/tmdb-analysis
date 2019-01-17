# -*- coding: utf-8 -*-
"""
Created on Mon Apr  2 14:10:13 2018

@author: jiliu
"""

import tmdbsimple as tmdb
tmdb.API_KEY = '96276d8a037f9e550a75c392a16693a5'

movie = tmdb.Movies(603)
response = movie.info()

movie.title
movie.budget

movie.genres


response = movie.releases()
for c in movie.countries:
    if c['iso_3166_1'] == 'US':
        print(c['certification'])
        
        
response = movie.alternative_titles()


search = tmdb.Search()
response = search.movie(query = 'The Bourne')

for s in search.results:
    print(s['title'], s['id'], s['release_date'], s['popularity'])
    
    
tvshow = tmdb.TV(75000)
response = tvshow.info()

tvshow.name


response = movie.releases()
for c in movie.countries:
    if c['iso_3166_1'] == 'US':
        print(c['certification'])
        
        
response = movie.alternative_titles()


search = tmdb.Search()
response = search.movie(query = 'The Bourne')

for s in search.results:
    print(s['title'], s['id'], s['release_date'], s['popularity'])