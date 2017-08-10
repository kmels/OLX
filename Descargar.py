import requests
from bs4 import BeautifulSoup
import os
import traceback
import sys
#######
### MONGO
######
import pymongo
from pymongo import MongoClient
from pymongo.helpers import DuplicateKeyError
#client = MongoClient('localhost', 27017)
client = MongoClient('kummer', 27827)
db = client.olx
db_items = db.en_alqulier

########
### FOTOS
########
import urllib
def descargar_imagen(obj_id, comunidad, url):   
    #print 'Descargando imagen ', obj_id, comunidad, url
    dname = "fotos/"+comunidad+"/"+str(obj_id)
    #print dname
    if not os.path.exists(dname):
        os.makedirs(dname)
    f = open(dname + "/" + url.split('/')[-1],'wb')
    f.write(urllib.urlopen(url).read())
    f.close()

#######
### UTILES
######
de_clase = lambda tag, clase: 'class' in tag.attrs and clase in tag.attrs['class']

from datetime import timedelta
from datetime import datetime

def obtener_fecha(date_str):
    assert type(date_str) is str, 'Date %s is type %s' % (date_str,type(date_str))    
    date = date_str.split(',')
    delta = timedelta(hours=0, minutes=0)
    today = datetime.now().replace(microsecond=0,second=0)
    
    if len(date) > 1:
        day = date[0].strip()
        if day == 'Hoy':
            day = today.replace(hour=0,minute=0)
        elif day == 'Ayer':
            day = today.replace(hour=0,minute=0) - timedelta(days=1)
        
        delta = datetime.strptime(date[1].strip(), '%H:%M')
        delta = timedelta(hours=delta.hour,minutes=delta.minute)
    else:
        date = date_str.split(' ')
        day = int(date[0])

        month = {'Jun':6,'Mayo': 5, 'Abr': 4, 'Mar': 3, 'Feb': 2, 'Ene': 1, 'Dic': 12, 'Nov': 11, 'Oct': 10, 'Sep': 9, 'Ago': 8, 'Jul': 7}[date[1].strip()]

        year = today.year
        if month < today.month:
            year -= 1
        day = datetime(year, month, day)
    
    try:
        return day + delta
    except:
        print('Fallo al obtener fecha con %s' % date_str)
        print("Exception in user code:")
        print('-'*60)
        exc_type, exc_value, exc_traceback = sys.exc_info()
        traceback.print_exception(exc_type, exc_value, exc_traceback)
        print('-'*60)

sopa = lambda url: BeautifulSoup(requests.get(url).text, 'html.parser') 
#########
#### LISTADO
#########
def listar_items(comunidad, categoria, pagina = 1):
    url = "https://%s.olx.com.gt/%s-p-%s" % (comunidad, categoria, pagina)    
    print("Visitando pagina %s - URL: %s" % (pagina, url))
    pagina = sopa(url)
    return list(filter(lambda li: 'class' in li.attrs and 'item' in li.attrs['class'], pagina.find_all('li')))

def zone_from_title(title):
    import re
    rgx = re.search('ZONA\s*(\d+)', title.upper())
    if not rgx:
        return None
    return int(rgx.groups()[0])

def price_gtq(price_tag):
    if 'Q' in price_tag:
        return price_tag.replace('Q','').replace(' ','').strip()
    return None

def price_usd(price_tag):
    if 'USD' in price_tag:
        return price_tag.replace('$','').replace('USD','').replace(' ','').strip()
    return None
    
def optional_map(key,value):
    if key == 'surface':
        return (key, value.replace('m2','').strip())
    return (key,value)
    
#######
### Items
######
def obtener_item(item_class):
    # Metadata de la ref    
    try:
        item_ref = item_class.find('a')
        title = item_ref.get('title')
        paragraphs = iter(item_ref.find_all('p'))
        tiene_optionals = len(list(filter(lambda p: 'class' in p.attrs and 'optionals' in p.attrs['class'], list(item_ref.find_all('p'))))) > 0
        if tiene_optionals:
            optionals = next(paragraphs).find_all('span')
            optclass = lambda atrs: next(c for c in atrs['class'] if 'optional' not in c and 'icon' not in c)
            #optionals = dict(list(map(lambda o: optional_map(o), optionals)))
            optionals = dict(list(map(lambda o: optional_map(optclass(o.attrs), o.text), optionals)))
#            optclass(o.attrs), o.text
        else:
            optionals = dict()
        price = iter(next(paragraphs).children)
        price_tag = next(price).strip()
        price_type = next(price, None)
        price_type = price_type.text.strip() if price_type is not None else None
        date = next(paragraphs).text.strip()
        thumb = item_ref.find('img')
        return {
            'title': title,
            'href': "http:" + item_ref.get('href'),
            'thumbnail': thumb.get('src') if thumb is not None else None,
            'optionals': optionals,
            'zone': zone_from_title(title),
            'price_usd': price_usd(price_tag),
            'price_gtq': price_gtq(price_tag),
            'price_type': price_type,
            'date': obtener_fecha(str(date)),
            'featured': 'class' in item_class.attrs and 'featuredad' in item_class.attrs['class']
        }
    except StopIteration:
        print('No se pudo obtener item de: ' + item_class)
        print('Paragraphs: ', list(item_class.find('a').find_all('p')))
        print("Exception in user code:")
        print('-'*60)
        exc_type, exc_value, exc_traceback = sys.exc_info()
        traceback.print_exception(exc_type, exc_value, exc_traceback)
        print('-'*60)
        return None
    
def descargar_item(item):

    print('DESCARGANDO!!!!')
    print(item)
    page = sopa(item['href'])
    main = page.find('main')    
    fotos = main.find_all('nav')[1]        

    article = main.find('article')
    divs = article.find_all('div')
    detalles = next(filter(lambda d: de_clase(d,'details'),divs))
    #opcionales = next(filter(lambda u: de_clase(u, 'item_partials_optionals_view'), article.find_all('ul')))
    desc = next(filter(lambda u: de_clase(u, 'item_partials_description_view'), article.find_all('p')))
    
    mapa = next(filter(lambda f:de_clase(f, 'map'), article.find_all('div')))
    if mapa is not None:
        [lati, longi] = mapa.find('a').get('href').split('/')[-1].split(',')
    
    item['fotos'] = list(filter(lambda href: href.endswith('.jpg'), map(lambda a: a.get('href'), fotos.find_all('a'))))
    item['town'] = detalles.find('span').text
    #item['optionals'] = dict(map(lambda li: (li.find('strong').text, li.find('span').text), opcionales.find_all('li')))
    item['desc'] = desc.text
    if mapa is not None:
        item['latitude'] = lati
        item['longitude'] = longi
    return item

def sha256(s):
    import hashlib, binascii
    dk = hashlib.pbkdf2_hmac('sha256', bytes(s,'utf-8'), b'OLX', 100000)
    return binascii.hexlify(dk)
    
def guardar_item(item, con_fotos = True):
    item_hash = sha256(str(item))
    item['item_hash'] = item_hash
    print('Guardando ... ', item_hash )
    print(item)
    
    try:
        doc_id = db_items.insert_one(item).inserted_id
    
        # descargar fotos ...
        if con_fotos:
            for foto_url in item['fotos']:
                comunidad = item['href'].split('.')[0]
                descargar_imagen(doc_id, comunidad, foto_url)
    
        return doc_id
    except DuplicateKeyError:
        return None

def obtener_items(desde = datetime.now(), limite = 20, direction = pymongo.ASCENDING, ignorar_featured = True):
    return list(db_items.find({'date': { '$lt': desde }, 'featured': False}, limit=limite).sort("date", direction))

def actualizar_items(comunidad = "ciudaddeguatemala", categoria = "piso-casa-en-venta-cat-367", siguiente_pagina = 1):
    item_mas_nuevo = obtener_items(limite=1, direction = pymongo.DESCENDING)
    item_mas_viejo = obtener_items(limite=1, direction = pymongo.ASCENDING)
    
    if len(item_mas_viejo) is 0:
        item_mas_viejo = None
        item_mas_nuevo = None
    else:
        item_mas_viejo = item_mas_viejo[-1]
        item_mas_nuevo = item_mas_nuevo[-1]
        print('El item mas viejo es del ', item_mas_viejo['date'])
        print('El item mas nuevo es del ', item_mas_nuevo['date'])

    historia_agotada = False
    
    while not historia_agotada:
        prev = datetime.now()
        print('Descargando pagina %s ' % siguiente_pagina)
        siguientes_items = listar_items(comunidad, categoria, pagina = siguiente_pagina)
        print("Numero de items: %s " % len(siguientes_items))
        siguiente_pagina += 1 
        siguientes_items = list(map(obtener_item, siguientes_items))
        print("Numero de items: %s " % len(siguientes_items))
        ### Encontrar el siguiente de forma cronologica
        siguiente_item = next(iter([i for i in siguientes_items if i is not None and not i['featured']]), None)
        siguiente_item = siguiente_item['date']
        
        print("Viendo")

        # si el ultimo ya esta guardado, parar
        if item_mas_nuevo is not None:
            if siguiente_item < item_mas_viejo['date']:
                print('Parando aqui. El siguiente es del %s, el mas nuevo registrado fue del %s' % (siguiente_item, item_mas_nuevo['date']))
                historia_agotada = True
                continue 
        #if item_mas_viejo is not None:
            #if siguiente_item < item_mas_viejo['date']:
            #    print 'Parando aqui. El siguiente es del %s, el mas viejo registrado fue del %s' % (siguiente_item, item_mas_viejo['date'])
        #        historia_agotada = True
        #        continue 
        
        for item in siguientes_items:
            try:
                now = datetime.now()
                print('Descargando ...', item['href'])
                #item = obtener_item(sigitem)                
                print("\"%s\"" % item['title'], '...', end='')
                item = descargar_item(item)
                print(len(item['fotos']), 'fotos ...',end='')
                doc_id = guardar_item(item, con_fotos=False)
                print('guardado como', doc_id,end='')
                print('(%s)' % item['date'], end='')
                print("(en %.2f s)" % (datetime.now() - now).total_seconds(),end='')
            except Exception as e:
                print('Fallo en la pagina %s con %s ' % (siguiente_pagina-1, item['href']),end='')
                print("Exception in user code:")
                print('-'*60)
                exc_type, exc_value, exc_traceback = sys.exc_info()
                traceback.print_exception(exc_type, exc_value, exc_traceback)
                print('-'*60)

        print("Pagina %s fue procesada en %.2f s" % (siguiente_pagina - 1, (datetime.now() - prev).total_seconds()))

def to_row(item):
    date = item['date'].strftime('%Y/%m/%d %H:%M')
    title = item['title'].replace(',','.')
    href = item.get('href','')
    lat = item.get('latitude','')
    lon = item.get('longitude','')
    optionals = item.get('optionals',{})
    bathrooms = optionals.get('bathrooms','')
    bedrooms = optionals.get('bedrooms','')
    surface = optionals.get('surface','')
    price_usd = item.get('price_usd','')
    price_gtq = item.get('price_gtq','')
    zone = item.get('zone','')
    cs = [date, zone, price_usd, price_gtq, surface, , bathrooms, bedrooms, title, href, lat, lon]
    _str = lambda x: str(x) if x else ''
    return ','.join(map(_str,cs))


#def guardar_comunidad_desde_pagina(comunidad, pagina):
    
#item_refs = pagina_items(sopa(url))

#guardar_item(item_refs[0])

# http://dabomart.com/inmobiliaria/casas-apartamento-en-venta
# https://vilia.com/gt/casas/venta
