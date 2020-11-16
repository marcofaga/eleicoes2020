import scrapy
from scrapy.utils.markup import remove_tags
from scrapy.exceptions import CloseSpider
from scrapy.spiders import Spider
import datetime, sys, functions as func, json
from scrapy.selector import Selector
import re

class estBA(scrapy.Spider):
    name = 'estBA'
    custom_settings = {'ITEM_PIPELINES': {'covidrobo.pipelines.CovidroboPipeline': 300}}
    urls = 'http://www.bahia.ba.gov.br/category/noticias/page/{}/'
    level = 'state'
    area = 'BA'
    dataIni = func.Functions().getLast(name)
    if dataIni == 0:
        dataIni = 1577840131
    termos = ('COVID',
        'covid',
        'coronavirus',
        'CORONAVIRUS',
        'COVID-19',
        'covid-19',
        'Covid-19',
        'Covid',
        'coronavírus',
        'Coronavírus')

    def start_requests(self):
        yield scrapy.Request((self.urls.format(1)),
          meta={'pageNumber': 1})

    def parse(self, response):
        linksnews = response.css('div.td-ss-main-content ::attr(href)').getall()
        linksnews = [i for i in linksnews if "category" not in i]
        linksnews = list(set(linksnews))
   
        for link in linksnews:
            yield scrapy.Request(url=link, callback=(self.parse2), meta={'url': link})

        pageNumber = response.meta['pageNumber']
        nextPN = pageNumber + 1
        yield scrapy.Request((self.urls.format(nextPN)),
          meta={'pageNumber': nextPN})

    def parse2(self, response):
        url = response.meta.get('url')

        getData = response.css('time::attr(datetime)').get()      
        getData = func.Functions().arrText(str(getData))
        getData = datetime.datetime.strptime(getData, '%Y-%m-%dT%H:%M:%S-03:00')
        getData = datetime.datetime.timestamp(getData)

        if getData < self.dataIni:
                print("********** AQUIIIII **********")
                print("*")
                print("*")
                print(url)
                print("*")
                print("*")
                print("******************************")
                raise CloseSpider('Limite de data atingido')

        getTit = response.css('h1.entry-title::text').get()
        getTit = func.Functions().arrText(str(getTit))

        getSub = "ND"
        #getSub = func.Functions().arrText(str(getSub))

        getText = response.css('div.td-post-content p::text').getall()
        getText = [remove_tags(item) for item in getText]
        str1 = ''
        for item in getText:
            str1 += ' ' + item

        getText = func.Functions().arrText(str1)

        time = func.Functions().getTS()

        if any((x in getText for x in self.termos)):

            jsonF = {

             'timescrap':time, 
             'robot':self.name, 
             'level':self.level, 
             'area':self.area, 
             'title':getTit, 
             'subtitle':getSub, 
             'date':getData, 
             'text':getText, 
             'url':url
            }
            
            yield jsonF
