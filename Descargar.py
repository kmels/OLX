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
client = MongoClient('localhost', 27017)
db = client.olx
db_items = db.items

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
        print 'Fallo al obtener fecha con %s' % date_str
        print "Exception in user code:"
        print '-'*60
        exc_type, exc_value, exc_traceback = sys.exc_info()
        traceback.print_exception(exc_type, exc_value, exc_traceback)
        print '-'*60

sopa = lambda url: BeautifulSoup(requests.get(url).text, 'html.parser') 
#########
#### LISTADO
#########
def listar_items(comunidad = "ciudaddeguatemala", pagina = 1):
    url = "https://%s.olx.com.gt/piso-casa-en-venta-cat-367-p-%s" % (comunidad, pagina)    
    pagina = sopa(url)
    return filter(lambda li: 'class' in li.attrs and 'item' in li.attrs['class'], pagina.find_all('li'))
    
#######
### Items
######
def obtener_item(item_class):
    # Metadata de la ref    
    item_ref = item_class.find('a')    
    paragraphs = iter(item_ref.find_all('p'))
    optionals = next(paragraphs).find_all('span')
    optclass = lambda atrs: next(c for c in atrs['class'] if 'optional' not in c and 'icon' not in c)
    optionals = dict(map(lambda o: (optclass(o.attrs), o.text), optionals))
    price = iter(next(paragraphs).children)
    price_tag = next(price).strip()
    price_type = next(price, None)
    price_type = price_type.text.strip() if price_type is not None else None
    date = next(paragraphs).text.strip()

    return {
        'title': item_ref.get('title'),
        'href': "http:" + item_ref.get('href'),
        'thumbnail': item_ref.find('img').get('src'),
        'optionals': optionals,
        'price_tag': price_tag,
        'price_type': price_type,
        'date': obtener_fecha(str(date)),
        'featured': 'class' in item_class.attrs and 'featuredad' in item_class.attrs['class']
    }
    
def descargar_item(item):
    page = sopa(item['href'])
    main = page.find('main')    
    fotos = main.find_all('nav')[1]        

    article = main.find('article')
    divs = article.find_all('div')
    detalles = filter(lambda d: de_clase(d,'details'),divs)[0]    
    opcionales = filter(lambda u: de_clase(u, 'item_partials_optionals_view'), article.find_all('ul'))[0]
    desc = filter(lambda u: de_clase(u, 'item_partials_description_view'), article.find_all('p'))[0]    
    
    mapa = filter(lambda f:de_clase(f, 'map'), article.find_all('div'))[0]
    if mapa is not None:
        [lati, longi] = mapa.find('a').get('href').split('/')[-1].split(',')
    
    item['fotos'] = filter(lambda href: href.endswith('.jpg'), map(lambda a: a.get('href'), fotos.find_all('a')))
    item['town'] = detalles.find('span').text        
    item['properties'] = dict(map(lambda li: (li.find('strong').text, li.find('span').text), opcionales.find_all('li')))
    item['desc'] = desc.text
    if mapa is not None:
        item['latitude'] = lati
        item['longitude'] = longi
    return item

def sha256(s):
    import hashlib, binascii
    dk = hashlib.pbkdf2_hmac('sha256', s, b'OLX', 100000)
    return binascii.hexlify(dk)
    
def guardar_item(item, con_fotos = True):
    item_hash = sha256(str(item))
    item['item_hash'] = item_hash
    #print 'Guardando ... ', item_hash 
    
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

def actualizar_items():
    ultimo_item = obtener_items(limite=1, direction = pymongo.DESCENDING)
    
    if len(ultimo_item) is 0:
        ultimo_item = None
    else:
        ultimo_item = ultimo_item[-1]
        print 'El ultimo item es de ', ultimo_item['date']

    historia_agotada = False
    siguiente_pagina = 1
    
    while ultimo_item is None or not historia_agotada:
        prev = datetime.now()
        print 'Descargando pagina %s ' % siguiente_pagina
        siguientes_items = listar_items(pagina = siguiente_pagina)
        siguiente_pagina += 1 
        siguientes_items = map(obtener_item, siguientes_items)
        
        next_not_featured = next(iter([i for i in siguientes_items if not i['featured']]), None)

        # si el ultimo ya esta guardado, parar
        if ultimo_item is not None and next_not_featured['date'] < ultimo_item['date']:
            print 'Ignorando pagina (el primero tiene fecha %s): El ultimo item tiene fecha %s' % (next_not_featured['date'], ultimo_item['date'])
            historia_agotada = True
            continue

        for item in siguientes_items:
            try:
                now = datetime.now()
                print 'Descargando ...', item['href'],
                #item = obtener_item(sigitem)                
                print "\"%s\"" % item['title'], '...',
                item = descargar_item(item)
                print len(item['fotos']), 'fotos ...',
                doc_id = guardar_item(item, con_fotos=False)
                print 'guardado como', doc_id,
                print '(%s)' % item['date']
                if not item['featured']:
                    ultimo_item = item
                print "(en %.2f s)" % (datetime.now() - now).total_seconds()
            except Exception as e:
                print 'Fallo en la pagina %s con %s ' % (siguiente_pagina-1, item['href'])
                print "Exception in user code:"
                print '-'*60
                exc_type, exc_value, exc_traceback = sys.exc_info()
                traceback.print_exception(exc_type, exc_value, exc_traceback)
                print '-'*60

        print "Pagina %s fue procesada en %.2f s" % (siguiente_pagina - 1, (datetime.now() - prev).total_seconds())

#def guardar_comunidad_desde_pagina(comunidad, pagina):
    
#item_refs = pagina_items(sopa(url))

#guardar_item(item_refs[0])
