import requests
from bs4 import BeautifulSoup
import os

#######
### MONGO
######
from pymongo import MongoClient
client = MongoClient('localhost', 27017)
db = client.olx
db_items = db.items

########
### FOTOS
########
import urllib
def descargar_imagen(obj_id, comunidad, url):   
    print 'Descargando imagen ', obj_id, comunidad, url
    dname = "fotos/"+comunidad+"/"+str(obj_id)
    print dname
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
    date = date_str.split(',')
    delta = timedelta(hours=0, minutes=0)
    today = datetime.now().replace(microsecond=0,second=0)
    if len(date) > 1:
        day = date[0].strip()
        if day == 'Hoy':
            day = today.replace(hour=0,minute=0)
        elif day is 'Ayer':
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

    return day + delta

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
        'date': obtener_fecha(date)
    }
    
def guardar_item(item_ref):
    # 2. Item post
    casa = sopa("http:" + href)
    main = casa.find('main')
    
    # Encontrar fotos
    [_,fotos,_] = main.find_all('nav')
    fotos = filter(lambda href: href.endswith('.jpg'), map(lambda a: a.get('href'), fotos.find_all('a')))    
    
    # detalles del post
    article = main.find('article')
    divs = article.find_all('div')
    detalles = filter(lambda d: de_clase(d,'details'),divs)[0]

    titulo = article.find('h1').text.strip()
    fecha = article.find('time').text
    lugar = detalles.find('span').text
    precio = detalles.find('strong').text

    # opcionales del post
    opcionales = filter(lambda u: de_clase(u, 'item_partials_optionals_view'), article.find_all('ul'))[0]
    opcionales = dict(map(lambda li: (li.find('strong').text, li.find('span').text), opcionales.find_all('li')))

    # descripcion del post
    desc = filter(lambda u: de_clase(u, 'item_partials_description_view'), article.find_all('p'))[0]    
    desc = desc.text

    # localidad
    mapa = filter(lambda f:de_clase(f, 'map'), article.find_all('div'))[0]

    [lati, longi] = mapa.find('a').get('href').split('/')[-1].split(',')

    print 'Guardando ... ',
    
    doc = {
        'titulo': titulo,
        'fecha': fecha,
        'lugar': lugar,
        'precio': precio,
        'opcionales': opcionales,
        'desc': desc,
        'mapa': {'lati': lati, 'longi': longi}
    }

    print doc['titulo'], 

    doc_id = db_items.insert_one(doc).inserted_id

    print doc_id

    # descargar fotos ...
    for foto_url in fotos:
        comunidad = href.split('.')[0]        
        descargar_imagen(doc_id, comunidad, foto_url)
    return doc

#item_refs = pagina_items(sopa(url))

#guardar_item(item_refs[0])
