module Web.View.Messages.Show where
import Web.View.Prelude

data ShowView = ShowView { signature :: Text, date :: Day, fileName :: Text}

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Sigature for File</h1>

        <div>
            <p><b> File signature: </b> </p>
            <div class="boxed"> 
                {signature}
            </div>
        </div> 
        <br/>

        <p><b> Date of signature: </b> {date} </p>
        <p><b> Name of file: </b> {fileName} </p>

    |]
        where
            breadcrumb = renderBreadcrumb
                [   breadcrumbLink "Index" IndexAction,
                    breadcrumbLink "New Message Signature" NewMessageAction,
                    breadcrumbText "Show Message Signaure"
                ]