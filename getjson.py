import scrapy
#from scrapy.utils.markup import remove_tags
#from scrapy.exceptions import CloseSpider
#from scrapy.spiders import Spider
#import datetime, sys, functions as func, json
#from scrapy.selector import Selector
#import re

class getjson(scrapy.Spider):

    custom_settings = {'ITEM_PIPELINES': {'covidrobo.pipelines.CovidroboPipeline': 300}}
    urls = ["https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados/ac/ac01120-c0013-e000426-v.json",
            "https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados/ac/ac01570-c0013-e000426-v.json",
            "https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/426/dados/ac/ac01058-c0013-e000426-v.json"]
                
    
    def start_requests(self):
        yield scrapy.Request(self.urls)

