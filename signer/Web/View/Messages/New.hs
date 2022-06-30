module Web.View.Messages.New where
import Web.View.Prelude

data NewView = NewView {}

instance View NewView where
    html NewView { } = [hsx|
        {breadcrumb}
        <h1>Sign new file</h1>


        <form method="POST" action="/CreateMessage" id="" class="new-form">

            <div class="form-group" id="form-group-message_file">
                <label for="file">File to Sign </label>
                <br/>
                <input    
                    type="file"
                    name="file"
                    accept="application/pdf, image/jpg, image/png, text/plain"
                >
            </div>


            <button class="btn btn-primary">Create Message</button>
        </form>

    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "New Message"
                ]

-- renderForm :: Message -> Html
-- renderForm message = formFor message [hsx|
--     {(textField #text)}
--     {submitButton}

-- |]