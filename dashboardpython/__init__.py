# 1)python -m dashboardpython poner en consola para que funcione
import dash
import seaborn as sns
import dash_html_components as html
import dash_core_components as dcc
from dash.dependencies import Input, Output
app=dash.Dash("dashboarddemarisa")
app.layout = html.Div( #Div es caja en blanco
    children=[
        html.H1("Este es mi primer dashboard en pyhton"),#H1 header1
        html.P(children="En este dashboard vamos a filtrar por sexo", id="miparrafo"), #P parrafo, le ponemos children porq ese texto es hijo de p no es necesario esplicitarlo pero es mejor
        #le doy un identificador para poder modificarlo después, el children es el nombre q le da python al parametro
        #sera su nombre aunque no lo especifiquempos
        dcc.Dropdown(#selector
            id="sexo",
            options=[
                {'label':'Hombre', 'value':'Male'},
                {'label':'Mujer', 'value':'Female'}
            ],
            value='Male' #valor por defecto, no es obligatorio
            ) 

    ]#hijos de objeto div
)
#Debajo del layout creamos funcion que se ejecutara cada vez que el usuario cambie el sexo del dropdown
@app.callback(
    Output(component_id="miparrafo", component_property="children"), #component_id id del componente q queremos cambiar en este caso la p
    #en component_property el atributo del componente que quiero cambiar
    [Input(component_id="sexo", component_property="value")] #los id de la info q le doy 
) #vas a decir a dash que quieres recibir y que quieres enviar
def cambiar_parrafo(sexo):
    """
    Esta función se ejecutara cada vez que el usuario cambie el parámetro sexo
    """
    return(sexo)
app.run_server(debug=True) #si sale error poner en consola python  dashboardpython/__init__.py en vez de 1) (fallo windows)

